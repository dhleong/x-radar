(ns ^{:author "Daniel Leong"
      :doc "Radar utilities"}
  xradar.radar-util
  (:require [quil.applet :refer [applet-close]]
            [xradar.util :refer [deep-merge]]))

(defn update-aircraft
  [radar craft]
  (swap! 
    radar 
    (fn [radar craft] 
      (let [cid (:cid craft)]
        (deep-merge radar {:aircraft {cid craft}})))
    craft))

(defn destroy-radar [radar]
  (applet-close (:sketch @radar)))

