(ns ^{:author "Daniel Leong"
      :doc "Weather (METAR) handling"}
  xradar.weather
  (:require [clojure.string :refer [join upper-case]]
            [quil.core :as q]
            [asfiled.metar :refer [decode-metar]]
            [xradar
             [radar-util :refer [redraw]]
             [util :refer [with-alpha]]]))

(def text-size 10)

;; static cache of weather, because
;;  it is the same globally, 
;;  regardless of which radar state
;;  you're in.
(defonce known-weather (atom {}))

;; static as above; list of airport ICAOs
;;  that we're watching
(defonce weather-watches (atom []))

;; as above; list of ICAOs whose
;;  wind or altimeter have changed
;;  since last rendering
(defonce pending-ack (atom #{}))

(defn- digits
  [number length]
  (format (str "%0" length "d") number))

;;
;; Information reading
;;

(defn acked?
  "Check if any changes to the given airport's METAR
  have been acked"
  [icao]
  (not (contains? @pending-ack (upper-case icao))))

(defn metar-for
  "Retrieves the decoded METAR for the given airport icao,
  if known. The raw metar is accessible with the :raw key."
  [icao]
  (get @known-weather (upper-case icao)))

(defn altimeter-for
  "Retrieves the altimeter setting for the given 
  airport icao, if known"
  [icao]
  (when-let [altim (:altimeter (metar-for icao))]
    (str altim)))

(defn winds-for
  "Retreives the winds for the given airport icao, if known"
  [icao]
  (when-let [wind (:wind (metar-for icao))]
    (let [variable (map digits (:dir-variable wind) (repeat 3))
          dir (cond
                (= :variable (:dir wind)) "VRB"
                (seq variable) (join "V" variable)
                :else (digits (:dir wind) 3))
          speed (if (:gust wind)
                  (str (digits (:speed wind) 2) "G" (digits (:gust wind) 2))
                  (digits (:speed wind) 2))]
      (str dir speed "KT"))))

;;
;; State updates
;;

(defn mark-acked!
  "Mark changed weather as acked"
  [icao]
  (swap! pending-ack disj (upper-case icao)))

(defn receive-metar!
  "Call when a METAR is received"
  [state metar]
  (let [decoded (decode-metar metar)
        icao (upper-case (:icao decoded))
        old (get @known-weather icao)]
    (when (or
            (not= (:wind old) (:wind decoded))
            (not= (:altimeter old) (:altimeter decoded)))
      (swap! known-weather 
             assoc 
             icao 
             (assoc decoded :raw metar))
      (swap! pending-ack conj icao)))
  (redraw state))

(defn reset-weather!
  "Clear all known weather data (as on disconnect)"
  []
  (swap! known-weather (constantly {}))
  (swap! pending-ack (constantly #{})))

(defn unwatch-weather!
  "Cancel request for updates to weather at a given airport"
  [icao]
  (let [upper-icao (upper-case icao)
        pred (partial = upper-icao)] 
    (swap! weather-watches 
           (fn [from] (vec (remove pred from))))))

(defn watch-weather!
  "Request updates to weather at a given airport"
  [icao]
  (swap! weather-watches conj (upper-case icao)))


;;
;; Artistry
;;

(defn draw-weather
  "Draws the weather and shifts the matrix to 
  start to the right of the last weather box
  (like all top-bar things should)."
  [radar]
  (q/text-size text-size)
  (q/rect-mode :corner)
  (let [scheme (-> radar :profile :scheme)
        height (+ (q/text-ascent)
                  (q/text-descent))]
    ;; are we showing a metar?
    (when-let [shown-metar (:shown-metar radar)]
      (q/text-size text-size)
      (let [metar (str
                    " "
                    (or (:raw (metar-for shown-metar)) "---")
                    " ")]
        (q/with-translation [0 height]
          (with-alpha q/fill-int (-> scheme :output :background))
          (q/stroke-int (-> scheme :output :text))
          (q/rect-mode :corner)
          (q/rect 0 0 (q/text-width metar) height)
          (q/fill-int (-> scheme :output :text))
          (q/text metar 0 text-size))))
    (doseq [icao @weather-watches]
      (let [line (str " " icao ": " 
                      (or (winds-for icao) "---")
                      " "
                      (or (altimeter-for icao) "---")
                      " ")
            width (q/text-width line)
            is-acked? (acked? icao)
            color (if is-acked?
                    (-> scheme :output :text)
                    (-> scheme :output :private)
                    )]
        (q/no-stroke)
        (with-alpha q/fill-int (-> scheme :output :background))
        (q/rect 0 0 width height)
        (q/fill-int color)
        (q/stroke-int color)
        (q/text line 0 text-size)
        (q/no-fill)
        (q/stroke-weight 1)
        (q/rect 0 0 width height)
        (q/translate [width 0])))))

