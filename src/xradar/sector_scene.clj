(ns ^{:author "Daniel Leong"
      :doc "XScene implementation for .sct2 files"}
  xradar.sector-scene
  (:require [clojure.string :refer [split]]
            [clojure.java.io :as io]
            [quil
             [core :as q]]
            [xradar.scene :refer [XScene draw-scene]]))

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

(defn- parse-data-line
  [data line]
  (cond
    ;; define
    (.startsWith line "#define")
    (let [parts (split line #" ")]
      (assoc-in data
                [:colors (keyword (second parts))]
                (parse-color (last parts))))
    ;; mode line
    (.startsWith line "[")
    (assoc data :mode- (.substring line 
                          1
                          (.indexOf line "]")))
    ;; TODO
    ))

(defn- clean-line
  [line]
  (let [comment-idx (.indexOf line ";")]
    (if (>= comment-idx 0)
      (-> line (.substring 0 comment-idx))
      line)))

(defn- load-from-reader
  [reader]
  (loop [lines (line-seq reader)
         data {}]
    (if (empty? lines)
      data
      (if-let [updated (parse-data-line data (clean-line (first lines)))]
        ; success!
        (recur (rest lines)
               updated)
        ; empty line
        (recur (rest lines)
               data)))))

(deftype SectorScene [data]
  XScene
  (draw-scene [this profile]
    ;; FIXME draw the scene
    nil))

(defn load-sector-data [input]
  (with-open [reader (io/reader input)]
    (load-from-reader reader)))

(defn load-sector [input]
  (->SectorScene (load-sector-data input)))
