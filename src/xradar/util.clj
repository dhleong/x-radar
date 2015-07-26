(ns ^{:author "Daniel Leong"
      :doc "Utilities"}
  xradar.util
  (:require [quil.core :as q]))

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
    (and (>= sx 0)
         (>= sy 0)
         (<= sx w)
         (<= sy h))))

