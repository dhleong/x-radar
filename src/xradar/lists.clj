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
    :widths [8 1 4 4]
    :source (source-for-fields :arrive :arrivals)}
   :departures 
   {:title "Departures"
    :fields [:callsign "D" :depart :squawk]
    :widths [8 1 4 4]
    :source (source-for-fields :depart :departures)}})

(defn create-lists
  []
  {:lists (atom #{})})

(defn items-for
  [radar source]
  (let [source-fn (get-in list-metas [source :source])]
    (source-fn radar)))

(defmulti render-value 
  "Get the raw value for the field of the given entry"
  (fn [radar field entry] (class field)))
(defmethod render-value java.lang.String
  [radar field entry]
  field)
(defmethod render-value clojure.lang.Keyword
  [radar field entry]
  (field entry))
(defmethod render-value :default
  [radar field entry]
  ;; assume it's a function
  (field radar entry))

(defn render-list-entry
  "Returns a sequence of strings for each
  field in the entry"
  [radar source-config entry]
  (map 
    (fn [k w] 
      (let [value (render-value radar k entry)]
        (format (str "%-" w "s") value)))
    (:fields source-config)
    (:widths source-config)))

(defn render-lists
  [state]
  ;; TODO
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
