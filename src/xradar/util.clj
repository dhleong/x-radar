(ns ^{:author "Daniel Leong"
      :doc "Utilities"}
  xradar.util)

(defn deep-merge [base-map new-map]
  (if (nil? new-map)
    ;; just use the base
    base-map
    ;; do the merge
    (merge-with 
      (fn [x y]
        (cond (and (map? x) (map? x)) (merge x y)
              (and (vector? x) (vector? x)) (concat x y) 
              :else y)) 
      base-map new-map)))
