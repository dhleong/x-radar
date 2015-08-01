(ns ^{:author "Daniel Leong"
      :doc "Rendering of the textual 'output'"}
  xradar.output
  (:require [clojure.string :refer [lower-case split]]
            [clojure.java.io :as io]
            [clj-time.local :as l]
            [quil.core :as q]
            [xradar
             [radar-util :refer [redraw]]
             [util :refer [with-alpha]]]))

(def output-padding 10)
(def output-size 14)

;; eg: `[00:00:00] ` but as all spaces
(def multi-line-prefix "           ")

(defn append-output
  "Append a line of output"
  [radar text & {:keys [color]}]
  (let [output (:output-buffer @radar)]
    (swap! output 
           (fn [buf new-entry] 
             (cons new-entry buf))
           {:text text
            :time (l/format-local-time 
                    (l/local-now) 
                    :hour-minute-second)
            :color color})
    (redraw radar)))

(defn format-text
  "Splits text into multiple lines as necessary"
  [chars-per-line line]
  (let [color (:color line)
        lines (partition-all (- chars-per-line 
                                (count multi-line-prefix))
                             (:text line))
        time-text (str "[" (:time line) "] ") 
        time-line {:color color
                   :text (str time-text
                              (apply str (first lines)))}
        other-lines (rest lines)]
      (-> (cons 
            time-line
            (map
              (fn [text]
                {:color color
                 :text (apply str multi-line-prefix text)})
              other-lines))
          reverse)))

;; public mostly for testing
(defn build-output
  [max-lines chars-per-line output-buffer]
  (->> output-buffer
       (take max-lines)
       (mapcat format-text 
               (repeat chars-per-line))
       (take max-lines)))

(defn resolve-color
  "Resolve the color to use to render the line.
  Public mostly for testing"
  [scheme line]
  (let [color (:color line)]
    (cond
      (keyword? color) (-> scheme :output color)
      (integer? color) color 
      :else (-> scheme :output :text))))

(defn draw-output
  [radar]
  (q/text-size output-size)
  (q/text-align :left)
  (q/rect-mode :corner)
  (let [scheme (-> radar :profile :scheme)
        max-output-count (-> radar :profile :output-size)
        max-output (* output-size max-output-count)
        char-width (q/text-width "M")
        available-width (- (q/width) output-padding output-padding)
        chars-per-line (int (Math/floor (/ available-width char-width)))]
    (with-alpha q/fill-int (-> scheme :output :background))
    (q/no-stroke)
    (q/rect output-padding 
            (- output-padding
               max-output)
            available-width
            max-output)
    (q/fill-int (-> scheme :output :text))
    (loop [output (build-output 
                    max-output-count
                    chars-per-line
                    @(:output-buffer radar))
           offset 0]
      (when (seq output)
        (let [line (first output)]
          (q/fill-int (resolve-color scheme line))
          (q/text (:text line) 
                  output-padding 
                  (- offset)))
        (when (< offset max-output)
          (recur (rest output)
                 (+ offset output-size)))))))
