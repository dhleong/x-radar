(ns ^{:author "Daniel Leong"
      :doc "Rendering of the textual 'output'"}
  xradar.output
  (:require [clojure.string :refer [lower-case split]]
            [clojure.java.io :as io]
            [quil.core :as q]
            [xradar.radar-util :refer [redraw]]))

(def output-padding 10)
(def output-size 14)

(defn append
  "Append a line of output"
  [radar text & {:keys [color]}]
  (let [output (:output-buffer @radar)]
    (swap! output 
           (fn [buf new-entry] 
             (cons new-entry buf))
           {:text text
            :color color})
    (redraw radar)))

(defn draw-output
  [radar]
  (let [max-output-count (-> @radar :profile :output-size)
        max-output (* output-size max-output-count)]
    (q/text-size output-size)
    (q/text-align :left)
    (loop [output @(:output-buffer @radar)
           offset 0]
      (when (seq output)
        (q/text (first output) 
                output-padding 
                (- (q/height) 
                   output-padding
                   offset))
        (when (< offset max-output)
          (recur (rest output)
                (+ offset output-size)))))))
