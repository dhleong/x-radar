(ns ^{:author "Daniel Leong"
      :doc "Rendering of flight strips"}
  xradar.flight-strips
  (:require [clojure.string :refer [lower-case split]]
            [clojure.java.io :as io]
            [clj-time.local :as l]
            [quil.core :as q]
            [xradar
             [radar-util :refer [redraw]]
             [util :refer [with-alpha]]]))

(def strip-width 500)
(def strip-height 60)
(def strip-border 2)
(def strip-padding 5)
(def half-padding (/ strip-padding 2))
(def text-size 12)

(defn render-strip
  "Renders the strip at the 'current location.'
  In other words, you must translate as appropriate
  so that the current 0,0 is the desired upper-left
  corner of the strip. The strip should have some
  extra fields not normally associated with an aircraft:
  :flight-type <:departure|:arrival|:local|:vfr|:over>
  :selected <bool>"
  [scheme craft]
  (let [flight-type (get craft :flight-type :unknown)
        background (-> scheme :strips flight-type)
        foreground (-> scheme :strips :foreground)
        col1-width (q/text-width "ACA1234B")
        col2-width (q/text-width "FL123 ")
        route-width (- strip-width 
                       strip-border strip-border
                       strip-padding strip-padding
                       col1-width col2-width col2-width)]
    (if (:selected craft)
      (q/stroke-int (-> scheme :strips :selected-border))
      (q/stroke-int (-> scheme :strips :border)))
    (with-alpha q/fill-int background)
    (q/stroke-weight strip-border)
    (q/rect 0 0 strip-width strip-height)
    (q/fill-int foreground)
    (q/text-size text-size)
    ;; column 1
    (q/translate (+ strip-padding strip-border) strip-border)
    (q/text (str (:callsign craft)) 
            0
            (+ strip-padding text-size))
    (q/text (str (:type craft)) 
            0
            (* 2 (+ strip-padding text-size)))
    (q/text (str (:cid craft)) 
            0
            (* 3 (+ strip-padding text-size)))
    ;; column 2
    (q/translate col1-width 0)
    (q/text (str (:squawk craft)) 
            0
            (+ strip-padding text-size))
    (q/text (str (:temp-alt craft)) 
            0
            (* 2 (+ strip-padding text-size)))
    (q/text (str (:cruise craft)) 
            0
            (* 3 (+ strip-padding text-size)))
    ;; column 2
    (q/translate col2-width 0)
    (q/text (str (:depart craft)) 
            0
            (+ half-padding text-size))
    (q/text (str (:arrive craft)) 
            0
            (+ half-padding (* 2 text-size)))
    (q/text (str (:alternate craft)) 
            0
            (+ half-padding (* 3 text-size)))
    (q/text (str (:scratch craft)) 
            0
            (+ half-padding (* 4 text-size)))
    ;; column 3 (route/remarks)
    (q/translate col2-width 0)
    (q/text (str (:route craft))
            0 half-padding
            route-width (* 3 text-size))
    (q/text (str (:remarks craft))
            0 (+ half-padding (* 3 text-size))
            route-width (* 4 text-size))
    ))

(defn render-strip-bay
  "Render the flight strip bay"
  [radar strips]
  ;; TODO
  (let [scheme (-> radar :profile :scheme)
        aircraft (-> radar :aircraft)]
    (when-let [craft (first (vals aircraft))]
      (q/with-translation [10 50]
        (render-strip scheme
                      (assoc craft 
                             ;; :flight-type :vfr
                             :selected false))))))
