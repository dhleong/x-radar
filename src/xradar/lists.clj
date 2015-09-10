(ns ^{:author "Daniel Leong"
      :doc "Rendering of data lists"}
  xradar.lists
  (:require [clojure.string :refer [join upper-case]]
            [quil.core :as q]
            [xradar
             [notif :refer [ack-attention!]]
             [util :refer [with-alpha]]]))

(defn source-for-fields
  [craft-key fields-key]
  (fn [radar]
    (let [fields (-> radar :profile fields-key)]
      (->> (:aircraft radar)
           vals
           (filter #(contains? fields (craft-key %)))))))

;; each data source function is provided with
;;  the radar map (IE: NOT the state)
(def list-metas
  {:arrivals 
   {:title "Arrivals"
    :fields [:callsign "A" :arrive "TODO"] ;; TODO func to calculate distance
    :source (source-for-fields :arrive :arrivals)}
   :departures 
   {:title "Departures"
    :fields [:callsign "D" :depart :squawk]
    :source (source-for-fields :depart :departures)}})

(defn create-lists
  []
  (atom #{}))

(defn items-for
  [radar source]
  (let [source-fn (get-in list-metas [source :source])]
    (source-fn radar)))

(defn render-lists
  [state]
  nil)

(defn toggle-list
  [state source]
  (when-not (contains? list-metas source)
    (throw (RuntimeException. (str "No data source " source))))
  (if (contains? (:lists @state))
    (do
      (swap! (:lists state) conj source)
      (ack-attention! source))
    (swap! (:lists state) disj source)))
