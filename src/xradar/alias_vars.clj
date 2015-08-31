(ns ^{:author "Daniel Leong"
      :doc "Variables and functions for use in aliases"}
  xradar.alias-vars
  (:require [clojure.string :refer [lower-case upper-case]]
            [clj-time.format :as f]
            [xradar
             [network :refer [my-callsign]]
             [weather :refer [altimeter-for metar-for winds-for]]]))

(defn- get-aircraft-var
  [state var-name]
  (when-let [selected (:selected @state)]
    (get-in @state [:aircraft selected var-name])))

(defn- aircraft-var
  [var-name]
  (fn [state]
    (get-aircraft-var state var-name)))

(def alias-variables
  {:squawk (aircraft-var :squawk)
   :route (aircraft-var :route)
   :arr (aircraft-var :arrive)
   :dep (aircraft-var :depart)
   :cruise (aircraft-var :cruise)
   :calt (aircraft-var :altitude)
   :aircraft (aircraft-var :callsign)
   :callsign (fn [state]
               (my-callsign (:network @state)))
   :winds (fn [state]
            (when-let [dep (get-aircraft-var :depart)]
              (winds-for dep)))
   ;; TODO there are still plenty of vars we could support
   })

(def alias-functions
  {:lc #(lower-case %2)
   :uc #(upper-case %2)
   :metar #(:raw (metar-for %2))
   :altim #(altimeter-for %2)
   :wind #(altimeter-for %2)
   ;; TODO many functions
   })
