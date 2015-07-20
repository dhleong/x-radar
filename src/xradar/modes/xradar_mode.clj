(ns ^{:author "Daniel Leong"
      :doc "Default Radar Mode implementation"}
  xradar.modes.xradar-mode
  (:require [quil.core :as q]
            [xradar.mode :refer :all]))

(def craft-width 10)
(def craft-height 10)

(defmulti my-aircraft (fn [scheme craft] (:state craft :untracked)))
(defmethod my-aircraft :untracked [scheme craft]
  (q/ellipse (:x craft) (:y craft) craft-width craft-height))

(deftype XRadarMode []
  RadarMode
  (draw-aircraft [this scheme aircraft]
    (let [state (:state aircraft :untracked)]
      (q/stroke (get-in scheme [:aircraft state]))
      (my-aircraft scheme aircraft))))

(defn create-mode [] (XRadarMode.))
