(ns ^{:author "Daniel Leong"
      :doc "Radar utilities"}
  xradar.radar-util
  (:require [quil
             [core :as q]
             [applet :refer [applet-close]]]
            [seesaw.core :refer [invoke-later]]
            [xradar.util :refer [deep-merge map-coord]]))

(defn get-location
  "Get the location on the screen of the radar window"
  [radar-atom]
  (let [point (-> @radar-atom
                  :sketch
                  .getLocationOnScreen)]
    {:x (.getX point) :y (.getY point)}))

(defn redraw
  "Schedule a redraw from anywhere"
  [radar-atom]
  (if-let [sketch (:sketch @radar-atom)]
    (.redraw sketch)))

(defn update-aircraft
  [radar-atom craft]
  (swap! 
    radar-atom
    (fn [radar craft] 
      (let [cid (:cid craft)
            mapped-coord (map-coord (-> radar :scene) craft)
            mapped (merge craft mapped-coord)]
        (redraw radar-atom)
        (deep-merge radar {:aircraft {cid mapped}})))
    craft))

(defn destroy-radar [radar]
  (applet-close (:sketch @radar)))

