(ns ^{:author "Daniel Leong"
      :doc "Aircraft utility functions"}
  xradar.aircraft
  (:require [clojure.string :refer [join]]
            [quil.core :as q]
            [xradar
             [lists :refer [distance-to-arrival]]
             [util :refer [bearing-to distance-between object-for]]]))

;;
;; Internal util
;;

(defn- coord-for
  [obj]
  (if (contains? obj :coord)
    (:coord obj)
    obj))

;;
;; Public utils
;;

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

;;
;; Bearing and distance-related
;;

(defn create-bd
  []
  {:bd-list (atom [])})

(defn draw-bd
  [radar]
  (q/stroke-int 0xffFFFFFF) ;; TODO 
  (q/fill-int 0xffFFFFFF) ;; TODO 
  (q/text-size 12)
  (q/text-align :center :bottom)
  (doseq [entry @(:bd-list radar)]
    (let [[from to] (vec entry)
          from-obj (object-for radar from)
          to-obj (object-for radar to)]
      (when (and from-obj to-obj)
        (let [c1 (coord-for from-obj)
              x1 (:x c1)
              y1 (:y c1)
              c2 (coord-for to-obj)
              x2 (:x c2)
              y2 (:y c2)
              mx (/ (+ x2 x1) 2)
              my (/ (+ y2 y1) 2)
              smx (q/screen-x mx my)
              smy (q/screen-y mx my)
              bearing (bearing-to c1 c2)]
          (q/line x1 y1 x2 y2)
          ;; draw the distance nicely
          (q/push-matrix)
          (q/camera)
          (q/hint :disable-depth-test) ; so we draw on top of scenery
          ;; TODO draw bearings for each to the other?
          (q/with-translation [smx smy]
            (q/with-rotation [bearing]
              (q/text
                (format "%.1fnm" (distance-between c1 c2))
                0 0)))
          (q/hint :enable-depth-test)
          (q/pop-matrix))))))

(defn toggle-bd-between
  "Toggle the rendering of bearing and distance
  between two objects"
  [state from to]
  (let [bdl (:bd-list @state)
        last-bdl @bdl
        new-entry #{from to}]
    (if (some #{new-entry} last-bdl)
      (swap! bdl (partial remove (partial = new-entry)))
      (swap! bdl conj #{from to}))))
