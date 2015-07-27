(ns ^{:author "Daniel Leong"
      :doc "Utilities"}
  xradar.util
  (:require [quil.core :as q]
            [clojure.core.matrix :refer [matrix inner-product set-current-implementation]]
            [xradar.scene :refer [get-center get-lon-scale loaded?]]))

(set-current-implementation :vectorz) 

(def coord-scale 10000)

(defn deep-merge [base-map new-map]
  (if (nil? new-map)
    ;; just use the base
    base-map
    ;; do the merge
    (merge-with 
      (fn [x y]
        (cond (and (map? x) (map? x)) (deep-merge x y)
              (and (vector? x) (vector? x)) (concat x y) 
              :else y)) 
      base-map new-map)))

(defn in-bounds
  "Check if a coordinate is within the bounds
  of the currently visible area"
  [x y]
  (let [w (q/width)
        h (q/height)
        sx (q/screen-x x y)
        sy (q/screen-y x y)]
    (or (and (>= sx 0)
             (>= sy 0))
        (and (<= sx w)
             (<= sy h)))))

;;
;; Wacky hacks
;;

(defn- my-get-center
  [scene-or-data]
  (if (map? scene-or-data)
    (-> scene-or-data :info :center)
    (get-center scene-or-data)))

(defn- my-get-lon-scale
  [scene-or-data]
  (if (map? scene-or-data)
    (if-let [info (:info scene-or-data)]
      (/ (:nm-per-lon info) (:nm-per-lat info))
      1)
    (get-lon-scale scene-or-data)))

(def cached-mat nil)
(defn- get-matrix
  [scene]
  (if-let [cached cached-mat]
    cached
    (when-let [point (my-get-center scene)]
      (when-let [sx (* coord-scale (my-get-lon-scale scene))]
        (let [px (:x point)
              py (:y point)
              sy coord-scale ;; just one (scaled)
              mat (matrix
                    [[sx 0  (- px (* sx px))]
                     [0  sy (- py (* sy py))]
                     [0  0  1]])]
          (def cached-mat mat)
          mat)))))

(defn map-coord
  "Used inside drawing functions to map a
  {:x, :y} coord as appropriate"
  [scene coord]
  (if (or (map? scene) (loaded? scene))
    ;; loaded!
    {:x (* (:x coord) coord-scale (my-get-lon-scale scene))
     :y (* (:y coord) coord-scale)}
    #_(if-let [mat (get-matrix scene)]
      (let [mapped 
            (inner-product
              mat
              [(:x coord) (:y coord) 1])]
        {:x (first mapped) :y (second mapped)})
      ;; no matrix, probably tests; just pass through
      coord)
    ;; not loaded; pass through
    coord))
