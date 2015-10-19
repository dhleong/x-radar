(ns ^{:author "Daniel Leong"
      :doc "XScene implementation for .sct2 files"}
  xradar.sector-scene
  (:require [clojure.string :refer [lower-case split]]
            [clojure.java.io :as io]
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

;;
;; Util methods
;;

(defn as-int
  [raw]
  (Integer/parseInt raw))

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
         parts (-> coord
                   (.substring 1)
                   (split #"\.")
                   (#(map as-int %)))]
     (* sign (+ (nth parts 0) ; degrees
                (/ (nth parts 1) 60) ; minutes
                (/ (nth parts 2) 3600) ; seconds
                (/ (nth parts 3) 3600000))))) ; decimal seconds
  ([scene lat lon]
   (map-coord scene {:x (parse-coord lon) :y (parse-coord lat)})))

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
    (case (:mode- data)
      :info (parse-info-line data line)
      :vor (parse-point-line :vor data line)
      :ndb (parse-point-line :ndb data line)
      :airport (parse-airport-line data line)
      :geo (parse-geo-line data line)
      :labels (parse-label-line data line)
      :runway (lparse-runway-line data line)
      ;; unsupported section
      nil)))

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
  (if (empty? shape)
    shape
    (loop [points shape
           last-min-x Long/MAX_VALUE
           last-min-y Long/MAX_VALUE
           last-max-x Long/MIN_VALUE
           last-max-y Long/MIN_VALUE]
      (let [next-points (rest points)
            this-point (first points)
            this-x (:x this-point)
            this-y (:y this-point)
            ;; update bounds
            new-min-x (min last-min-x this-x)
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
                 new-max-x new-max-y))))))

(defn- parse-shapes
  [data]
  (if-let [geo-data (:geo data)]
    (loop [geo geo-data
           shapes []
           iterations 1
           this-shape []]
      (let [last-coord (last this-shape)
            last-meta (meta this-shape)
            last-color (:color last-meta)
            last-x (:x last-coord)
            last-y (:y last-coord)
            next-geos (rest geo)
            next-line (first geo)
            next-color (:color next-line)
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
              (conj this-shape (:end next-line)) ;; append next coord
              (with-meta [(:start next-line)
                          (:end next-line)]
                         {:color next-color}))]
        (if (empty? next-geos)
          (assoc data :geo-shapes (conj next-shapes 
                                        (with-bounds next-shape)))
          (recur next-geos next-shapes (inc iterations) next-shape))))
    ;; no geo data; don't do anything
    data))

(defn load-sector-data [input]
  (with-open [reader (io/reader input)]
    (parse-shapes (load-from-reader reader))))

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
  [mode artist]
  `(doseq [element# (get ~'data ~mode)]
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

(defn- do-draw-scene
  "Separate for easier tweaking in repl"
  [data profile this-zoom]
  (let [start (System/currentTimeMillis)]
    (doseq [mode (-> profile :draw)]
      (when-not (occlude-zoom mode this-zoom)
        (case mode
          :geo (draw-each :geo-shapes draw-shape)
          :labels (draw-each :labels draw-label)
          :runway (draw-each :runway draw-runway)
          ;; else, unsupported type
          nil)))
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
      (when-let [info (-> @data-atom :info)]
        (->> [:airport :ndb :vor]
             (mapcat #(get @data-atom % []))
             (filter #(.equalsIgnoreCase point-name (:name %)))
             first))))
  (loaded? [this]
    (not (empty? @data-atom))))
  
(defn load-sector [input & callback]
  (let [data-atom (atom {})
        scene (->SectorScene data-atom)]
    (def last-scene-atom data-atom) ;; NB  for testing purposes
    (future (do
              (swap! data-atom (fn [_] (load-sector-data input)))
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
