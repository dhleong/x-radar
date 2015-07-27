(ns ^{:author "Daniel Leong"
      :doc "Radar utilities"}
  xradar.radar-util
  (:require [quil.applet :refer [applet-close]]
            [xradar.util :refer [deep-merge map-coord]]))

(defn update-aircraft
  [radar-atom craft]
  (swap! 
    radar-atom
    (fn [radar craft] 
      (let [cid (:cid craft)
            mapped-coord (map-coord (-> radar :scene) craft)
            mapped (merge craft mapped-coord)]
        (deep-merge radar {:aircraft {cid mapped}})))
    craft))

(defn destroy-radar [radar]
  (applet-close (:sketch @radar)))

