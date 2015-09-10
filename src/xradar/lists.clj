(ns ^{:author "Daniel Leong"
      :doc "Rendering of data lists"}
  xradar.lists
  (:require [clojure.string :refer [join upper-case]]
            [quil.core :as q]
            [xradar
             [notif :refer [ack-attention!]]
             [util :refer [with-alpha]]]))

;;
;; Constants
;;

(def text-size 12)

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
  [radar]
  (q/text-size text-size)
  (let [scheme (-> radar :profile :scheme)
        text-width (q/text-width "M")
        height (+ (q/text-ascent)
                  (q/text-descent))
        lists (:lists radar)] 
    (doseq [source @lists]
      (let [info (source list-metas)
            col-widths (:widths info)
            total-chars (apply + 
                               (inc (count col-widths))
                               col-widths)
            width (* total-chars text-width)
            items (items-for radar source)]
        ;; title
        (q/rect-mode :corner)
        (q/stroke-int (-> scheme :output :outgoing))
        (with-alpha q/fill-int (-> scheme :output :background-highlight))
        (q/rect 0 0 width height)
        (q/fill-int (-> scheme :output :outgoing))
        (q/text (:title info) 8 text-size)
        (q/translate [0 height])
        (q/no-stroke)
        ;; no items?
        (when (empty? items)
          (with-alpha q/fill-int (-> scheme :output :background))
          (q/rect 0 0 width height)
          (q/fill-int (-> scheme :output :outgoing))
          (q/text "(None)" 8 text-size)
          (q/translate [0 height]))
        ;; some items
        (doseq [entry items]
          (let [parts (render-list-entry
                        radar
                        info
                        entry)]
            (with-alpha q/fill-int (-> scheme :output :background))
            (q/rect 0 0 width height)
            (q/fill-int (-> scheme :output :outgoing))
            (q/text (join " " parts) 8 text-size)
            (q/translate [0 height])))))))

(defn toggle-list
  [state source]
  (let [lists (:lists @state)] 
    (def last-lists lists)
    (def last-source source)
    (when-not (contains? list-metas source)
      (throw (RuntimeException. (str "No data source " source))))
    (if (contains? @lists source)
      ;; viewed; stop it
      (swap! lists disj source)
      ;; not viewed; view it
      (do
        (swap! lists conj source)
        (ack-attention! source))))
  (def updated-lists (:lists @state)))
