(ns ^{:author "Daniel Leong"
      :doc "Rendering of the textual 'output'"}
  xradar.output
  (:require [clojure.string :refer [lower-case split]]
            [clojure.java.io :as io]
            [clj-time.local :as l]
            [quil.core :as q]
            [xradar.radar-util :refer [redraw]]))

(def output-padding 10)
(def output-size 14)

(defn append-output
  "Append a line of output"
  [radar text & {:keys [color]}]
  (let [output (:output-buffer @radar)]
    (swap! output 
           (fn [buf new-entry] 
             (cons new-entry buf))
           {:text text
            :time (l/format-local-time (l/local-now) :hour-minute-second)
            :color color})
    (redraw radar)))

(defn format-text
  [available-width char-width line]
  ;; TODO split lines as necessary
  [{:color (:color line)
    :text (str "[" (:time line) "] " char-width (:text line))}])

(defn draw-output
  [radar]
  (q/text-size output-size)
  (q/text-align :left)
  (q/rect-mode :corner)
  (let [scheme (-> radar :profile :scheme)
        max-output-count (-> radar :profile :output-size)
        max-output (* output-size max-output-count)
        char-width (q/text-width "M")
        available-width (- (q/width) output-padding output-padding)]
    (q/fill-int (-> scheme :output :background) 200)
    (q/no-stroke)
    (q/rect output-padding 
            (- output-padding
               max-output)
            available-width
            max-output)
    (q/fill-int (-> scheme :output :text))
    (loop [output (->> @(:output-buffer radar)
                       (take max-output-count)
                       (mapcat format-text 
                               (repeat available-width)
                               (repeat char-width)))
           offset 0]
      (when (seq output)
        (let [line (first output)]
          (q/fill-int (or (:color (first output))
                          (-> scheme :output :text)))
          (q/text (:text line) 
                  output-padding 
                  (- offset)))
        (when (< offset max-output)
          (recur (rest output)
                 (+ offset output-size)))))))
