(ns xradar.core
  (:require [seesaw.core :refer [native!]]
            [xradar.radar :refer [create-radar update-aircraft]])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (native!)
  ;; TODO load profile
  ;; commented out for now for testing
  ;; (def radar (create-radar {}))
  ;; (update-aircraft radar {:cid 2 :x 40 :y 40})
  )
