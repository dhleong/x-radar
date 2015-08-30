(ns ^{:author "Daniel Leong"
      :doc "Variables and functions for use in aliases"}
  xradar.alias-vars
  (:require [clojure.string :refer [lower-case upper-case]]
            [clj-time.format :as f]
            [xradar.network :refer [my-callsign]]))

(defn- aircraft-var
  [var-name]
  (fn [state]
    (when-let [selected (:selected @state)]
      (get-in @state [:aircraft selected var-name]))))

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
   ;; TODO there are still plenty of vars we could support
   })

(def alias-functions
  {:lc #(lower-case %2)
   :uc #(upper-case %2)
   ;; TODO many functions
   })
