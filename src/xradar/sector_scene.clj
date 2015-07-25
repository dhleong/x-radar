(ns ^{:author "Daniel Leong"
      :doc "XScene implementation for .sct2 files"}
  xradar.sector-scene
  (:require [clojure.string :refer [lower-case split]]
            [clojure.java.io :as io]
            [quil
             [core :as q]]
            [xradar.scene :refer [XScene draw-scene]]))

;;
;; Constants
;;
(def re-spaces #"\s+")

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
           \N 1
           \E 1
           \S -1
           \W -1)
         parts (-> coord
                   (.substring 1)
                   (split #"\.")
                   (#(map as-int %)))]
     (* sign (+ (nth parts 0) ; degrees
                (/ (nth parts 1) 60) ; minutes
                (/ (nth parts 2) 3600) ; seconds
                (/ (nth parts 3) 3600000))))) ; decimal seconds
  ([lat lon]
   {:x (parse-coord lon) :y (parse-coord lat)}))

(defn- clean-line
  [line]
  (let [comment-idx (.indexOf line ";")]
    (if (>= comment-idx 0)
      (-> line (.substring 0 comment-idx))
      line)))

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
          3 [:center-lat line]
          4 [:center-lon line]
          5 [:nm-per-lat (Integer/parseInt line)]
          6 [:nm-per-lon (Integer/parseInt line)]
          7 [:magnet-var (Double/parseDouble line)]
          8 [:sect-scale line])]
    (assoc-in (assoc data :info-line- (inc info-line))
              [:info k] v)))

(defn- parse-point-line
  "[VOR] or [NDB]"
  [section data line]
  (let [parts (split line re-spaces)
        info
        {:name (first parts)
         :freq (second parts)
         :coord (parse-coord (nth parts 2)
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
         :coord (parse-coord (nth parts 2)
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
          {:start (parse-coord (nth parts 0)
                               (nth parts 1))
           :end (parse-coord (nth parts 2)
                             (nth parts 3))
           :color (get-in data 
                          [:colors color] 
                          (parse-color (last parts)))}
          (catch IllegalArgumentException e
            ;; ZNY prefixes each group with a line
            ;;  referring to the airport's name....
            nil))]
    (assoc data 
           :geo 
           (conj (get data :geo []) info))))

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
         :coord (parse-coord (nth parts 0)
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

(defn load-sector-data [input]
  (with-open [reader (io/reader input)]
    (load-from-reader reader)))

;;
;; Public interface
;;

(deftype SectorScene [data]
  XScene
  (draw-scene [this profile]
    ;; FIXME draw the scene
    nil))

(defn load-sector [input]
  (->SectorScene (load-sector-data input)))
