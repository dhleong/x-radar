(ns ^{:author "Daniel Leong"
      :doc "XScene implementation for .sct2 files"}
  xradar.sector-scene
  (:require [clojure.string :refer [lower-case split]]
            [clojure.java.io :as io]
            [kdtree :as kdt]
            [quil
             [core :as q]]
            [xradar
             [scene :refer :all]
             [util :refer [coord-scale deep-merge in-bounds map-coord]]]))

;;
;; Constants
;;
(def re-spaces #"\s+")
(def latlon-scale-plus 1)
(def latlon-scale-minus -1)

;; the lists of data with the following
;;  types will be indexed in a KD-tree
(def kd-index-keys [:label :geo])

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

(defn- kd-index-item
  [item]
  (if-let [coord (:coord item)]
    [(with-meta [(:x coord) (:y coord)] item)]
    (let [start (:start item)
          end (:end item)]
      [(with-meta [(:x start) (:y start)] item)
       (with-meta [(:x end) (:y end)] item)])))

(defn- kd-index 
  [data]
  (loop [result data
         kd-indexers kd-index-keys]
    (let [data-type (first kd-indexers)
          mapped
          (mapcat
            kd-index-item
            (get result data-type))
          mapped-key (keyword (str (name data-type) "-kd"))
          next-result (assoc result 
                             mapped-key 
                             (kdt/build-tree mapped))
          next-indexers (rest kd-indexers)]
      (if-not (seq next-indexers)
        next-result
        (recur next-result next-indexers)))))

(defn load-sector-data [input]
  (with-open [reader (io/reader input)]
    (kd-index (load-from-reader reader))))

;;
;; Art utils
;;

(defn- draw-line
  [line]
  (let 
    [x1 (:x (:start line))
        y1 (:y (:start line))
        x2 (:x (:end line))
        y2 (:y (:end line))]
    (when (or (in-bounds x1 y1) (in-bounds x2 y2))
      (q/stroke-int (:color line))
      (q/stroke-weight 1)
      (q/line x1 y1 x2 y2))))

(defn- draw-label
  [label]
  (let [{:keys [x y]} (:coord label)]
    (when (in-bounds x y)
      (q/fill-int (:color label))
      (q/text-size 3.4)
      (q/text-align :center :center)
      (q/text (:label label) x y))))

(defn- select-visible
  [radar data kd-tree]
  ;; FIXME this might grab too much
  (let [this-camera (or (:camera radar) (-> data :info :center))
        zoom (or (:zoom radar) 220)
        cam-x (:x this-camera)
        cam-y (:y this-camera)
        half-width (/ (q/width) 2)
        half-height (/ (q/height) 2)
        mapped-dimens (map-coord data {:x half-width :y half-height})
        mapped-width (/ (:x mapped-dimens) zoom)
        mapped-height (/ (:y mapped-dimens) zoom)
        upper-left  {:x (- cam-x mapped-width) 
                     :y (- cam-y mapped-height)}
        lower-right  {:x (+ cam-x mapped-width) 
                      :y (+ cam-y mapped-height)}] 
    ;; (def sel-range [mapped-width mapped-height])
    ;; (def compare-it [upper-left lower-right this-camera])
    ;; (def center [(q/screen-x (:x upper-left) (:y upper-left))
    ;;              (q/screen-y (:x upper-left) (:y upper-left))])
    (->> (kdt/interval-search
           kd-tree
           [[(:x upper-left) (:x lower-right)]
            [(:y upper-left) (:y lower-right)]])
         (map meta))))

(defn- draw-each
  [radar data mode artist]
  (let [base (get data mode)
        items (if (.endsWith (name mode) "-kd")
                (select-visible radar data base)
                base)]
    (doseq [element items]
      (try
        (artist element)
        (catch Exception e
          (throw (RuntimeException. 
                   (str "Error drawing " element " in " mode)
                   e)))))))

(defn- do-draw-scene
  "Separated out for easier updating in repl"
  [radar data profile]
  (let [start (System/currentTimeMillis)]
   (doseq [mode (-> profile :draw)]
     (case mode
       ;; TODO use :geo-kd
       :geo (draw-each radar data :geo draw-line)
       :labels (draw-each radar data :labels draw-label)
       ;; else, unsupported type
       nil))
   (def last-draw-time (- (System/currentTimeMillis) start))))

;;
;; Public interface
;;

(deftype SectorScene [data-atom]
  XScene
  (draw-scene [this radar profile]
    (if-let [data @data-atom]
      (do-draw-scene radar data profile)))
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
    (when-let [info (-> @data-atom :info)]
      (->> [:airport :ndb :vor]
           (mapcat #(get @data-atom % []))
           (filter #(.equalsIgnoreCase point-name (:name %)))
           first)))
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
