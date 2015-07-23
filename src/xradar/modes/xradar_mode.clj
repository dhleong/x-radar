(ns ^{:author "Daniel Leong"
      :doc "Default Radar Mode implementation"}
  xradar.modes.xradar-mode
  (:require [quil.core :as q]
            [xradar.mode :refer :all]))

(def craft-width 10)
(def craft-height 10)

(defmulti my-aircraft (fn [scheme craft] (:state craft :untracked)))
(defmethod my-aircraft :selected [scheme craft]
  (q/stroke-weight 1.5)
  (q/no-fill)
  (q/ellipse (:x craft) (:y craft) craft-width craft-height))
(defmethod my-aircraft :default [scheme craft]
  (q/stroke-weight 0.5)
  (q/no-fill)
  (q/ellipse (:x craft) (:y craft) craft-width craft-height))

(deftype XRadarMode []
  RadarMode
  (draw-aircraft [this scheme craft]
    (let [state (:state craft :untracked)]
      (q/stroke-int (get-in scheme [:aircraft state] (-> scheme :aircraft :untracked)))
      (my-aircraft scheme craft))))

(defn create-mode [] (XRadarMode.))
