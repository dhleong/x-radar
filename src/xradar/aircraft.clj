(ns ^{:author "Daniel Leong"
      :doc "Aircraft utility functions"}
  xradar.aircraft
  (:require [clojure.string :refer [join]]
            [xradar.lists :refer [distance-to-arrival]]))

(defn describe-craft
  "Given an aircraft object, return a brief
  textual description"
  [radar craft]
  ;; TODO we could perhaps lookup the aircraft type,
  ;; arriving airport full name, etc.
  (join
    " " 
    [(:type craft)
     (str (:depart craft) "->" (:arrive craft))
     (distance-to-arrival radar craft)
     (str "C@" (:cruise craft))]))
