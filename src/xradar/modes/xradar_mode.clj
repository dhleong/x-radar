(ns ^{:author "Daniel Leong"
      :doc "Default Radar Mode implementation"}
  xradar.modes.xradar-mode
  (:require [quil.core :as q]
            [xradar.mode :refer :all]))

(def craft-width 10)
(def craft-height 10)

(defmulti my-aircraft (fn [scheme craft] (:state craft :untracked)))
(defmethod my-aircraft :default [scheme craft]
  (q/ellipse-mode :center)
  (q/ellipse (:x craft) (:y craft) craft-width craft-height))

(defn- do-draw-aircraft [radar scheme craft]
  (let [state (:state craft :untracked)
          craft-color (get-in scheme 
                              [:aircraft state]
                              (-> scheme :aircraft :untracked))
          mapping (if (= :select-aircraft (:mode radar))
                    (get-in radar [:craft-bindings (:cid craft)] nil))]
      (case state
        :selected (q/stroke-weight 1.5)
        ;; default
        (q/stroke-weight 0.5))
      (q/no-fill)
      (q/stroke-int craft-color)
      (my-aircraft scheme craft)
      (q/fill-int craft-color)
      (when mapping
        (q/fill-int craft-color)
        (q/text-size 14)
        (q/text-align :right :center)
        (q/text (str mapping) (- (:x craft) 10) (:y craft)))))

(deftype XRadarMode []
  RadarMode
  (draw-aircraft [this radar scheme craft]
    ;; split out so it's easier to update in repl
    (do-draw-aircraft radar scheme craft)))

(defn create-mode [] (XRadarMode.))
