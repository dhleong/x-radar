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

(def cached-mat nil)
(defn- get-matrix
  [scene]
  (if-let [cached cached-mat]
    cached
    (let [point (get-center scene)
         px (:x point)
         py (:y point)
         sx (* coord-scale (get-lon-scale scene))
         sy coord-scale ;; just one (scaled)
         mat (matrix
               [[sx 0  (- px (* sx px))]
                [0  sy (- py (* sy py))]
                [0  0  1]])]
     #_(swap! (q/state :radar-state) #(assoc % :map-matrix mat))
     (def cached-mat mat)
     mat)))

(defn map-coord
  "Used inside drawing functions to map a
  {:x, :y} coord as appropriate"
  [scene coord]
  (if (loaded? scene)
    ;; loaded!
    (let [mapped 
          (inner-product
            (get-matrix scene)
            [(:x coord) (:y coord) 1])]
      {:x (first mapped) :y (second mapped)})
    ;; not loaded; pass through
    coord))
