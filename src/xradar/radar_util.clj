(ns ^{:author "Daniel Leong"
      :doc "Radar utilities"}
  xradar.radar-util
  (:require [quil
             [core :as q]
             [applet :refer [applet-close]]]
            [seesaw.core :refer [invoke-later]]
            [xradar.util :refer [deep-merge map-coord]]))

(def max-history 5)

(defn osx-app []
  (try
    (let [app-class (Class/forName "com.apple.eawt.Application")
          getter (.getMethod app-class "getApplication" (into-array Class []))]
      (.invoke getter nil (into-array [])))
    (catch Exception e
      ;; not OSX, or OSX extensions missing
      (println e)
      nil)))

(defn get-location
  "Get the location on the screen of the radar window"
  [radar-atom]
  (let [point (-> @radar-atom
                  :sketch
                  .getLocationOnScreen)]
    {:x (.getX point) :y (.getY point)}))

(defn request-attention!
  "Request attention to the app when not focused"
  [& {:keys [is-critical] :or {is-critical false}}]
  ;; TODO how does windows/linux want to handle this?
  (when-let [app (osx-app)]
    (.requestUserAttention app is-critical)))

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
        (redraw radar-atom)
        (deep-merge radar {:aircraft {cid mapped}})))
    craft)
    ;; return the updated aircraft
    (get-in @radar-atom [:aircraft (:cid craft)]))

(defn destroy-radar [radar]
  (applet-close (:sketch @radar)))

