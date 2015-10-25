(ns ^{:author "Daniel Leong"
      :doc "XScene implementation for .sct2 files"}
  xradar.sector-scene
  (:require [clojure.string :refer [lower-case split]]
            [clojure.java.io :as io]
            [taoensso.timbre.profiling :as profiling
             :refer (pspy pspy* profile defnp p p*)]
            [quil
             [core :as q]]
            [xradar
             [scene :refer :all]
             [util :refer [coord-scale bearing-to deep-merge
                           in-bounds map-coord]]]))

;;
;; Constants
;;
(def re-spaces #"\s+")
(def latlon-scale-plus 1)
(def latlon-scale-minus -1)

(def diagram-name-length 26)

(defonce lazy-inflated-cache (atom {}))

;;
;; Util methods
;;

(defn as-int
  [raw]
  (Integer/parseInt raw))

(defn find-point-in 
  [data point-name]
  (->> [:airport :ndb :vor :fixes]
      (mapcat #(get data % []))
      (filter #(.equalsIgnoreCase point-name (:name %)))
      first))

(defn parse-color
  [raw]
  (try
    (let [int-val (Integer/parseInt raw)
          red (mod int-val 256)
          green (int (mod (/ int-val 256) 256))
          blue (int (mod (/ int-val 65536) 256))]
      (+ 0xff000000 ;; always 100% opaque
         (bit-shift-left red 16)
         (bit-shift-left green 8)
         blue))
    (catch NumberFormatException e
      ;; default to black, I guess?
      0xff000000)))

(defn parse-coord
  ([coord]
   (let [sign
         (case (first coord)
           ;; NB N and S are swapped from
           ;;  what you might expect since
           ;;  the origin is at the top instead of the bottom.
           ;;  We scale it up quite a bit so it can be drawn nicely
           \N latlon-scale-minus
           \E latlon-scale-plus
           \S latlon-scale-plus
           \W latlon-scale-minus)
         ; elegant but slow old method
         ;; parts (-> coord
         ;;           (.substring 1)
         ;;           (split #"\.")
         ;;           (#(map as-int %)))
         ;; degrees (nth parts 0)
         ;; minutes (nth parts 1)
         ;; seconds (nth parts 2)
         ;; dec-sec (nth parts 3 0)
         ; less-elegant but more-efficient new method
         ; It's DMS format; 
         ;  degrees: 3-digits wide
         ;  minutes: 2-digits wide
         ;  seconds: 2-digits wide
         ;  decimal: 3-4-digits wide
         ; and the Decimal is sometimes omitted
         degrees (as-int (subs coord 1 4))
         minutes (as-int (subs coord 5 7))
         seconds (as-int (subs coord 8 10))
         dec-sec (if (> (count coord) 11)  ;; is there a decimal component?
                   (as-int (subs coord 11))
                   0)
         ]
     (* sign (+ degrees ; degrees
                (/ minutes 60) ; minutes
                (/ seconds 3600) ; seconds
                (/ dec-sec 3600000))))) ; decimal seconds
  ([scene lat lon]
   (if (and (string? lat)
            (< (count lat) 6)
            (= lat lon))
     ;; waypoint
     (p :expand-waypoint 
        (when-let [point (if (map? scene)
                           (find-point-in scene lat)
                           (find-point scene lat))]
          (select-keys point [:x :y])))
     ;; coordinates
     (map-coord scene {:x (p :parse-single-coord (parse-coord lon)) 
                       :y (p :parse-single-coord (parse-coord lat))}))))

(defn- clean-line
  [line]
  (let [comment-idx (.indexOf line ";")]
    (if (>= comment-idx 0)
      (-> line (.substring 0 comment-idx))
      line)))

(defn- insert-center
  [data]
  ;; when we've ready everything, insert :center
  (if (-> data :info :sect-scale)
    ;; generate the :center value
    (let [lon-scale (/ (-> data :info :nm-per-lon) (-> data :info :nm-per-lat))] 
      (assoc-in data 
                [:info :center] 
                {:y (-> data :info :center-lat (* coord-scale))
                 :x (-> data :info :center-lon (* coord-scale lon-scale))}))
    ;; nothing to do
    data))

;;
;; Sections
;;

(defn- parse-info-line
  "[INFO]"
  [data line]
  (let [info-line (or (:info-line- data) 0)
        [k v] 
        (case info-line
          0 [:name line]
          1 [:default-callsign line]
          2 [:default-airport line]
          3 [:center-lat (parse-coord line)]
          4 [:center-lon (parse-coord line)]
          5 [:nm-per-lat (Integer/parseInt line)]
          6 [:nm-per-lon (Integer/parseInt line)]
          7 [:magnet-var (Double/parseDouble line)]
          8 [:sect-scale line])]
    (-> data
        (assoc :info-line- (inc info-line))
        (assoc-in [:info k] v)
        (insert-center))))

(defn- parse-point-line
  "[VOR] or [NDB]"
  [section data line]
  (let [parts (split line re-spaces)
        info
        {:name (first parts)
         :freq (second parts)
         :coord (parse-coord data
                             (nth parts 2)
                             (nth parts 3))}]
    (assoc data 
           section 
           (conj (get data section []) info))))

(defn- parse-airport-line
  "[AIRPORT]"
  [data line]
  (let [parts (split line re-spaces)
        info
        {:name (first parts)
         :freq (second parts)
         :coord (parse-coord data
                             (nth parts 2)
                             (nth parts 3))
         :airspace (last parts)}]
    (assoc data 
           :airport 
           (conj (get data :airport []) info))))

(defn- parse-geo-line
  "[GEO]"
  [data line]
  (let [parts (split line re-spaces)
        color (keyword (last parts))
        info
        (try
          {:start (parse-coord data
                               (nth parts 0)
                               (nth parts 1))
           :end (parse-coord data
                             (nth parts 2)
                             (nth parts 3))
           :color (get-in data 
                          [:colors color] 
                          (parse-color (last parts)))}
          (catch IllegalArgumentException e
            ;; ZNY prefixes each group with a line
            ;;  referring to the airport's name....
            nil))]
    (when (not (nil? info))
      (assoc data 
             :geo 
             (conj (get data :geo []) info)))))

(defn- parse-label-line
  "[LABELS]"
  [data line]
  (let [label-end (-> line (.indexOf "\"" 1))
        label (-> line (.substring 1 label-end))
        line-info (-> line
                      (.substring (inc label-end))
                      (.trim))
        parts (split line-info re-spaces)
        color (keyword (last parts))
        info
        {:label label
         :coord (parse-coord data
                             (nth parts 0)
                             (nth parts 1))
         :color (get-in data 
                        [:colors color] 
                        (parse-color (last parts)))}]
    (assoc data 
           :labels
           (conj (get data :labels []) info))))

(defn- parse-runway-line
  "[RUNWAY]"
  [data line]
  (let [parts (split line re-spaces)
        info 
        {:labels (take 2 parts)
         :magnetic (map
                     #(Integer/parseInt %)
                     (->> parts (drop 2) (take 2)))
         :start (apply parse-coord 
                       data
                       (->> parts (drop 4) (take 2)))
         :end (apply parse-coord 
                       data
                       (->> parts (drop 6) (take 2)))}]
    (assoc data 
           :runway
           (conj (get data :runway []) 
                 info))))

(defn- parse-fix-line
  "[FIXES]"
  [data line]
  (let [parts (split line re-spaces)
        info
        {:name (first parts)
         :coord (parse-coord data
                             (nth parts 1)
                             (nth parts 2))}]
    (assoc data 
           :fixes 
           (conj (get data :fixes []) info))))

(defn- parse-diagram-line
  "[SID] or [STAR]"
  [section data line]
  (let [diagram-name (-> line
                         (subs 0 diagram-name-length)
                         (.trim))
        parts (-> line
                  (subs diagram-name-length)
                  (.trim)
                  (split re-spaces))
        previous-vec (get data section [])
        color (keyword (last parts))
        info
        (try
          {:name (p :parse-name (when (empty? diagram-name)
                                  diagram-name))
           :start (p :parse-start (parse-coord data
                                               (nth parts 0)
                                               (nth parts 1)))
           :end (p :parse-end (parse-coord data
                                           (nth parts 2)
                                           (nth parts 3)))
           :color (p :parse-color (or
                                    (get-in data 
                                            [:colors color]) 
                                    (parse-color (last parts))))}
          (catch IllegalArgumentException e
            ;; ZNY prefixes each group with a line
            ;;  referring to the airport's name....
            nil))]
    (when (not (nil? info))
      (p :merge-data (assoc data 
                            section 
                            (conj previous-vec info))))))

(defn- lazy-parse-diagram-line
  [section data line]
  (let [diagram-name (-> line
                         (subs 0 diagram-name-length)
                         (.trim)
                         (#(when-not (empty? %)
                             %)))
        section-data (get data section {})
        info (if diagram-name
               (with-meta [line] {:name diagram-name})
               (when-let [last-name (:last-name section-data)]
                 (-> (get section-data last-name)
                     (conj line))))]
    (when-let [diag-name (:name (meta info))]
      (-> data
          (assoc-in [section diag-name] info)
          (assoc-in [section :last-name] diag-name)))))

;;
;; Parsing loop
;;

(defn- parse-data-line
  [data line]
  (cond
    ;; define
    (.startsWith line "#define")
    (let [parts (split line re-spaces)]
      (assoc-in data
                [:colors (keyword (second parts))]
                (parse-color (last parts))))
    ;; mode line
    (.startsWith line "[")
    (assoc data :mode- (-> line
                           (.substring 1 (.indexOf line "]"))
                           lower-case
                           (.replace " " "-")
                           keyword))
    ;; data line
    (not (or (empty? line) (.startsWith line ";")))
    (do
      (def parsing [(:mode- data) line])
      (case (:mode- data)
        :info (parse-info-line data line)
        :vor (parse-point-line :vor data line)
        :ndb (parse-point-line :ndb data line)
        :airport (parse-airport-line data line)
        :geo (parse-geo-line data line)
        :labels (parse-label-line data line)
        :runway (parse-runway-line data line)
        :fixes (parse-fix-line data line)
        :sid (lazy-parse-diagram-line :sid data line)
        :star (lazy-parse-diagram-line :star data line)
        ;; unsupported section
        nil))))

(defn- load-from-reader
  [reader]
  (loop [lines (line-seq reader)
         data {}]
    (if (empty? lines)
      ;; return what we have
      (dissoc data :mode- :info-line-)
      ;; parse it!
      (if-let [updated (parse-data-line data (clean-line (first lines)))]
        ; success!
        (recur (rest lines)
               updated)
        ; empty line
        (recur (rest lines)
               data)))))

(defn- with-bounds
  [shape]
  (cond
    (empty? shape) shape
    (= 1 (count shape)) shape
    :else
    (loop [points shape
           last-min-x Long/MAX_VALUE
           last-min-y Long/MAX_VALUE
           last-max-x Long/MIN_VALUE
           last-max-y Long/MIN_VALUE]
      (let [next-points (rest points)
            this-point (first points)
            this-x (:x this-point)
            this-y (:y this-point)]
        ;; update bounds
        (if (or (nil? this-x) (nil? this-y))
          (do
            ;; temp compat while we don't have
            ;;  FIXES parsing
            (def nil-shape [shape (meta shape)])
            shape)
          (let [new-min-x (min last-min-x this-x)
                new-min-y (min last-min-y this-y)
                new-max-x (max last-max-x this-x)
                new-max-y (max last-max-y this-y)]
            (if (empty? next-points)
              ;; we've seen it all! update the bounds
              (vary-meta shape 
                         assoc 
                         ;; ie: left, top, right, bottom
                         :bounds [new-min-x new-min-y 
                                  new-max-x new-max-y])
              ;; keep looking
              (recur next-points
                     new-min-x new-min-y
                     new-max-x new-max-y))))))))

(defn- parse-shapes
  [data in-type out-type & [known-name]]
  (def parsing-type in-type)
  (if-let [geo-data (in-type data)]
    (loop [geo geo-data
           shapes []
           iterations 1
           this-shape []]
      (let [last-coord (last this-shape)
            last-meta (meta this-shape)
            last-name (:name last-meta)
            last-color (:color last-meta)
            last-x (:x last-coord)
            last-y (:y last-coord)
            next-geos (rest geo)
            next-line (first geo)
            next-color (:color next-line)
            next-name (:name next-line)
            next-x (:x (:start next-line))
            next-y (:y (:start next-line))
            ;; do we continue the previous shape?
            shape-continues?
            (and (= next-x last-x)
                 (= next-y last-y)
                 (= next-color last-color))
            next-shapes
            (if shape-continues?
              shapes ;; no change yet
              (if (empty? this-shape)
                shapes ;; begin first shape ever
                (conj shapes 
                      (with-bounds this-shape)))) ;; append the new shape
            next-shape
            (if shape-continues?
              ;; append next coord
              (conj this-shape (:end next-line))
              ;; done
              (with-meta [(:start next-line)
                          (:end next-line)]
                         {:color next-color
                          :name (or known-name (:name next-line))}))]
        (cond
          ;; if the entire thing is 0, just drop it
          (apply = 0 (mapcat vals next-shape)) 
          (recur next-geos next-shapes (inc iterations) [])
          ;; nothing else? done!
          (empty? next-geos)
          (assoc data out-type 
                 (conj next-shapes 
                       (with-bounds next-shape)))
          ;; keep going
          :else
          (recur next-geos next-shapes (inc iterations) next-shape))))
    ;; no geo data; don't do anything
    data))

(defn load-sector-data [input]
  (with-open [reader (io/reader input)]
    (-> (load-from-reader reader)
        (parse-shapes :geo :geo-shapes)
        #_(parse-shapes :sid :sid-shapes)
        #_(parse-shapes :star :star-shapes))))

;;
;; Art utils
;;

(defmacro arity 
  [fun]
  `(->> (var ~fun)
        meta
        :arglists
        first
        count))

(defn- draw-line
  [line & [default-color]]
  (let 
    [x1 (:x (:start line))
     y1 (:y (:start line))
     x2 (:x (:end line))
     y2 (:y (:end line))]
    (when (or (in-bounds x1 y1) (in-bounds x2 y2))
      (q/stroke-int (:color line default-color))
      (q/stroke-weight 1)
      (q/line x1 y1 x2 y2)
      true)))

(defn- draw-label
  [label]
  (let [{:keys [x y]} (:coord label)]
    (when (in-bounds x y)
      (q/fill-int (:color label))
      (q/text-size 3.4)
      (q/text-align :center :center)
      (q/text (:label label) x y))))

(defn- draw-shape
  [shape]
  (let [data (meta shape)
        color (:color data)
        [l t r b] (:bounds data)]
    (when (in-bounds (:bounds data))
      (q/no-fill)
      (q/stroke-int color)
      (q/stroke-weight 1)
      (q/begin-shape)
      (doseq [vertex shape]
        (q/vertex (:x vertex) (:y vertex)))
      (q/end-shape))))

(defn- draw-runway
  [profile runway]
  (let [runway-color (-> profile :scheme :runway)]
    (when (draw-line runway runway-color)
      (let [start (:start runway)
            end (:end runway)
            x1 (:x start)
            y1 (:y start)
            x2 (:x end)
            y2 (:y end)
            rotation (bearing-to start end)]
        (q/text-align :center)
        (q/text-size 3)
        (q/fill-int runway-color)
        (q/with-translation [x1 y1]
          (q/with-rotation [(- rotation (/ Math/PI 2))]
           (q/text (first (:labels runway)) 0 0)))
        (q/with-translation [x2 y2]
          (q/with-rotation [(+ rotation (/ Math/PI 2))]
           (q/text (last (:labels runway)) 0 0)))))))

(defmacro draw-each
  [mode artist & [filter-fn]]
  `(doseq [element# ~(if filter-fn
                       `(filter 
                          ~filter-fn
                          (get ~'data ~mode) )
                       `(get ~'data ~mode))]
     (try
       (case (arity ~artist)
         1 (~artist element#)
         2 (~artist ~'profile element#))
       (catch Exception e#
         (def last-exc e#)
         (throw (RuntimeException. 
                  (str "Error drawing " ~mode ": " element#)
                  e#))))))

(defn- occlude-zoom
  "Return true if we should occlude the mode
  because of the zoom level"
  [mode this-zoom]
  (case mode
    :labels (> this-zoom 500)
    ;; anything else can just always be drawn
    nil))


(defn parse-lazy-diagram-lines
  "Given a lazy sequence of lines from
  a single diagram, parse it into a 
  mini data map"
  [section data lines]
  (if (empty? lines)
    data
    (recur section
           (parse-diagram-line 
             section data (first lines))
           (rest lines))))

(defn ensure-diagram-inflated
  [section data diagram-name]
  (when-not (get-in @lazy-inflated-cache 
                    [:inflated section diagram-name])
    (swap! 
      lazy-inflated-cache
      (fn [cache]
        (let [shapes-name (keyword (str (name section) "-shapes"))
              raw-lines (get-in data [section diagram-name])]
          (-> cache
              (assoc-in [:inflated section diagram-name] true)
              (assoc shapes-name
                     (concat
                       (get cache shapes-name [])
                       (-> (parse-lazy-diagram-lines 
                             section 
                             (p :filter-data (dissoc data section))
                             raw-lines)
                           (parse-shapes 
                             section shapes-name 
                             diagram-name) ;; provide the name
                           (get shapes-name))))))))))

(defn clear-inflated-cache
  []
  (swap! lazy-inflated-cache (constantly {})))

(defn- do-draw-scene
  "Separate for easier tweaking in repl"
  [data profile this-zoom]
  (let [start (System/currentTimeMillis)]
    (doseq [star (-> profile :stars)]
      (ensure-diagram-inflated :star data star))
    (doseq [sid (-> profile :sids)]
      (ensure-diagram-inflated :sid data sid))
    (doseq [mode (-> profile :draw)]
      (when-not (occlude-zoom mode this-zoom)
        (case mode
          :geo (draw-each :geo-shapes draw-shape)
          :labels (draw-each :labels draw-label)
          :runway (draw-each :runway draw-runway)
          ;; else, unsupported type
          nil)))
    (let [stars (hash-set (-> profile :stars))
          data @lazy-inflated-cache]
      (draw-each :star-shapes draw-shape
                (partial contains? stars)))
    (let [sids (hash-set (-> profile :sids))
          data @lazy-inflated-cache]
      (draw-each :sid-shapes draw-shape
                 (partial contains? sids)))
    (def duration (- (System/currentTimeMillis) start))))

;;
;; Public interface
;;

(deftype SectorScene [data-atom]
  XScene
  (draw-scene [this profile this-zoom]
    (if-let [data @data-atom]
      (do-draw-scene data profile this-zoom)))
  (get-center [this]
    (when-let [info (-> @data-atom :info)]
      (-> info :center)))
  (get-lon-scale [this]
    (if-let [info (-> @data-atom :info)]
      (/ (:nm-per-lon info) (:nm-per-lat info))
      1))
  (get-magnetic-var [this]
    (if-let [info (-> @data-atom :info)]
      (:magnet-var info)
      0))
  (find-point [this point-name]
    (when (string? point-name)
      (let [data @data-atom]
        (when-let [info (-> data :info)]
          (find-point-in data point-name)))))
  (loaded? [this]
    (not (empty? @data-atom))))
  
(defn load-sector [input & callback]
  (let [data-atom (atom {})
        scene (->SectorScene data-atom)]
    (def last-scene-atom data-atom) ;; NB  for testing purposes
    (future (do
              (swap! data-atom (fn [_] 
                                 (try
                                   (load-sector-data input)
                                   (catch Exception e
                                     (def last-read-exc e)))))
              (when (seq callback) 
                ((first callback)))))
    scene))

;; for testing...
(defn load-sector-sync [input]
  (let [data-atom (atom {})
        scene (->SectorScene data-atom)]
    ;; for testing
    (def last-scene-atom data-atom) ;; NB  for testing purposes
    @(future (swap! data-atom (fn [_] (load-sector-data input))))
    scene))


(defn- do-profile
  "Convenience function for profiling tests"
  [scene-data]
  (clear-inflated-cache)
  (profile :info :DiagramInflate
    (dotimes [n 50]
      (do
        (ensure-diagram-inflated :star scene-data "+ Class B - New York")
        (clear-inflated-cache)))))
