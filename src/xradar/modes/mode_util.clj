(ns ^{:author "Daniel Leong"
      :doc "Utility methods for radar mode implementations"}
  xradar.modes.mode-util
  (:require [clojure.test :refer [function?]]
            [quil.core :as q]))

(def mapping-offset-x (- 10))
(def mapping-offset-y 0)
(def mapping-text-size 14)

(defn do-draw-aircraft
  [radar scheme my-aircraft craft]
  (let [state (:state craft :untracked)
        craft-color (if (:selected craft)
                      (-> scheme :aircraft :selected)
                      (get-in scheme [:aircraft state]))
        mapping (if (= :select-aircraft (:mode radar))
                  (get-in radar [:craft-bindings (:cid craft)] nil))]
    (q/fill-int craft-color)
    (q/stroke-int craft-color)
    (let [cx (:x craft)
          cy (:y craft)
          x (q/screen-x cx cy 0)
          y (q/screen-y cx cy 0)]
      (q/push-matrix)
      (q/camera)
      (q/translate x y 0))
    (q/hint :disable-depth-test) ;; so we draw on top of scenery
    (my-aircraft scheme craft)
    (when mapping
      (q/fill-int craft-color)
      (q/text-size mapping-text-size)
      (q/text-align :right :center)
      (q/text (str mapping) mapping-offset-x mapping-offset-y))
    (q/hint :enable-depth-test)
    (q/pop-matrix)))

