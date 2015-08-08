(ns ^{:author "Daniel Leong"
      :doc "Default Radar Mode implementation"}
  xradar.modes.xradar-mode
  (:require [quil.core :as q]
            [xradar.mode :refer :all]
            [xradar.modes.mode-util :refer [do-draw-aircraft]]))

(def craft-width 10)
(def craft-height 10)

(defmulti my-aircraft (fn [scheme craft] (:state craft :untracked)))
(defmethod my-aircraft :default [scheme craft]
  (q/ellipse-mode :center)
  (q/ellipse 0 0 craft-width craft-height))

(deftype XRadarMode []
  RadarMode
  (draw-aircraft [this radar scheme craft]
    ;; we can simply wrap up our specific artist
    ;;  in the base draw method
    (do-draw-aircraft radar scheme my-aircraft craft)))

(defn create-mode [] (XRadarMode.))
