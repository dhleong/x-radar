(ns ^{:author "Daniel Leong"
      :doc "Weather (METAR) handling"}
  xradar.weather
  (:require [clojure.string :refer [join upper-case]]
            [asfiled.metar :refer [decode-metar]]))

;; static cache of weather, because
;;  it is the same globally, 
;;  regardless of which radar state
;;  you're in.
(def known-weather (atom {}))

(defn- digits
  [number length]
  (format (str "%0" length "d") number))

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

(defn receive-metar!
  "Call when a METAR is received"
  [metar]
  (let [decoded (decode-metar metar)]
    (swap! known-weather 
           assoc 
           (:icao decoded) 
           (assoc decoded :raw metar))))

(defn reset-weather!
  "Clear all known weather data (as on disconnect)"
  []
  (swap! known-weather (constantly {})))
