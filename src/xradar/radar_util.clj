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

(defn apply-concat
  [parts]
  (apply concat parts))

(defn strips
  "Returns the set of CIDs for strips being followed.
  Here to prevent circular dependencies (and also it's
  not commonly needed)"
  [state]
  (some-> @state
          :strips
          deref
          (select-keys [0 1])
          vals
          apply-concat
          set))

(defn request-attention-if-needed!
  [state craft]
  (let [radar @state
        aircraft (:aircraft radar)
        cid (:cid craft)] 
    ;; existing aircraft that finally gets an FP?
    (when (and
            (contains? (strips state) cid)
            (empty? (:route (get aircraft cid)))
            (not (empty? (:route craft))))
      (if (= :strips (:mode @(:input radar)))
        ;; when in strips mode, don't be topical
        (request-attention!
          :is-critical true)
        (request-attention!
          :topic :fp 
          :is-critical true)) 
      (redraw state))
    ;; new aircraft?
    (when-not (contains? aircraft cid)
      ;; new aircraft. Do we care about it?
      (let [profile (:profile radar)
            arrival? (contains? (:arrivals profile) (:arrive craft))
            departure? (contains? (:departures profile) (:arrive craft))]
        (when (or arrival? departure?)
          (request-attention! 
            :topic (if arrival? :arrivals :departures)
            :is-critical true))))))

(defn update-aircraft
  [state craft]
  (request-attention-if-needed! state craft)
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

