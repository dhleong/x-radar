(ns ^{:author "Daniel Leong"
      :doc "Radar utilities"}
  xradar.radar-util
  (:require [quil
             [core :as q]
             [applet :refer [applet-close]]]
            [seesaw.core :refer [invoke-later]]
            [xradar
             [notif :refer [request-attention!]]
             [util :refer [deep-merge map-coord]]]))

(def max-history 5)

(defn get-location
  "Get the location on the screen of the radar window"
  [state]
  (let [point (-> @state
                  :sketch
                  .getLocationOnScreen)]
    {:x (.getX point) :y (.getY point)}))

(defn redraw
  "Schedule a redraw from anywhere"
  [state]
  (if-let [sketch (:sketch @state)]
    (.redraw sketch)))

(defn update-aircraft
  [state craft]
  (when-not (contains? (:aircraft @state) (:cid craft))
    ;; new aircraft. Do we care about it?
    (let [profile (:profile @state)
          arrivals (:arrivals profile)
          departures (:departures profile)]
      (when (or (contains? arrivals (:arrive craft))
                (contains? departures (:depart craft)))
        (request-attention!))))
  (swap! 
    state
    (fn [radar craft] 
      (let [cid (:cid craft)
            old-craft (get-in radar [:aircraft cid])
            mapped-coord (map-coord (-> radar :scene) craft)
            history (when old-craft 
                      {:history 
                       (->> (:history old-craft [])
                            (cons  ; insert at the front
                                  {:x (:x old-craft)
                                   :y (:y old-craft)})
                            (take max-history))})
            mapped (merge craft history mapped-coord)]
        (redraw state)
        (deep-merge radar {:aircraft {cid mapped}})))
    craft)
    ;; return the updated aircraft
    (get-in @state [:aircraft (:cid craft)]))

(defn destroy-radar [radar]
  (applet-close (:sketch @radar)))

