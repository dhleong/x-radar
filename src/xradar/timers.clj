(ns ^{:author "Daniel Leong"
      :doc "Timer Utils"}
  xradar.timers
  (:require [quil.core :as q]
            [xradar.util :refer [with-alpha]]))

(def text-size 10)

;; global timers
(defonce all-timers (atom {}))

(def format-times-up " --:-- ")

(defn draw-timers 
  [radar]
  (q/text-size text-size)
  (q/rect-mode :corner)
  (let [scheme (-> radar :profile :scheme)
        height (+ (q/text-ascent)
                  (q/text-descent))]
    (q/fill-int (-> scheme :output :text))
    (doseq [[duration [end timer]] @all-timers]
      (let [delta (- end (System/currentTimeMillis))
            time-left? (> delta 0)
            line (str
                   " " duration "|"
                   (if time-left?
                     (format " %02d:%02d "
                             (int (/ delta 60000))
                             (int (/ (mod delta 60000) 1000)))
                     format-times-up))
            width (q/text-width line)
            color (cond
                    (< delta 0) (-> scheme :output :error)
                    (< delta 5000) (-> scheme :output :warning)
                    (< delta 30000) (-> scheme :output :outgoing)
                    :else (-> scheme :output :text) )]
        (q/no-stroke)
        (with-alpha q/fill-int (-> scheme :output :background))
        (q/rect 0 0 width height)
        (q/fill-int color)
        (q/stroke-int color)
        (q/text line 0 text-size)
        (q/no-fill)
        (q/stroke-weight 1)
        (q/rect 0 0 width height)
        (q/translate [width 0])))))

(defn toggle-timer 
  "Returns true if the timer was set, false if cancelled"
  [state duration-minutes]
  (let [millis (* duration-minutes 60000)
        end (+ (System/currentTimeMillis) millis)]
    (if-let [[_ timer] (get @all-timers duration-minutes)]
      (do
        ;; TODO Cancel the timer
        (swap! all-timers dissoc duration-minutes)
        false)
      (do
        (swap! all-timers assoc duration-minutes [end nil])  ;; TODO timer
        true))))
