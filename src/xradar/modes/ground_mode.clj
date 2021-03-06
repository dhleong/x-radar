(ns ^{:author "Daniel Leong"
      :doc "'Ground' Radar Mode implementation"}
  xradar.modes.ground-mode
  (:require [clojure.string :refer [lower-case]]
            [quil.core :as q]
            [xradar.mode :refer :all]
            [xradar.modes.mode-util :refer [do-draw-aircraft]]))

(def craft-radius-plus 3)
(def craft-radius-minus -3)
(def craft-width (* 2 craft-radius-plus))
(def craft-height (* 2 craft-radius-plus))

(def line-width 9)
(def line-margin 9)

(defn create-line1
  [craft]
  (let [sign (:callsign craft)
        remarks (lower-case (:remarks craft))]
    (cond
      (.contains remarks "/v") sign
      (.contains remarks "/r") (str sign "/r")
      (.contains remarks "/t") (str sign "/t")
      :else (str sign "/?"))))

(defn create-line2
  [craft]
  (let [base (->> (:type craft)
                  (take 4)
                  (apply str))]
    (cond
      ;; squawking vfr
      (= :vfr (:rules craft)) base
      ;; squawking incorrect code
      (and (= :normal (:squawking-mode craft))
           (not= (:squawk craft) (:squawking craft)))
      (str base " !" (.substring (:squawking craft) 0 2))
      ;; ground-speed, if available
      (number? (:ground-speed craft))
      (str base " " (:ground-speed craft))
      :else base)))

(defmulti draw-info-box (fn [scheme craft] (:state craft :untracked)))
(defmethod draw-info-box :default [scheme craft]
  ;; TODO box orientation and line rendering are probably
  ;;  a strong candidate for abstraction into mode-util
  (let [rot (or (:info-rotate craft) 0)
        rads (Math/toRadians rot)
        line-len (+ line-width (or (:info-length craft) 0))
        orientation (cond 
                      (<= 65 rot 115) :center
                      (<= 135 rot 225) :right
                      (<= 246 rot 295) :center
                      :else :left)
        offset (if (= :center orientation)
                 (* 2 craft-radius-plus)
                 0)]
    (when (= "ACA264" (:callsign craft))
      (def hi craft))
    (q/with-rotation [rads]
      (q/with-translation [(* 3 craft-radius-plus) 0]
        (q/stroke-weight 1)
        (q/line 0 0 line-len 0)
        (q/translate (+ line-len line-margin offset) 0)
        (q/with-rotation [(- rads)]
          (q/text-size 12)
          (q/text-align orientation :bottom)
          (q/text (create-line1 craft) 0 0)
          (q/text-align orientation :top)
          (q/text (create-line2 craft) 0 0))))))

(defmulti my-aircraft (fn [scheme craft] (:state craft :untracked)))
(defmethod my-aircraft :tracked [scheme craft]
  (q/text-align :center :center)
  (q/text-size 14)
  (q/text (:tracked-by craft) 0 (* -0.18 (q/text-ascent))))
(defmethod my-aircraft :untracked [scheme craft]
  (q/stroke-weight 1)
  (cond
    (= :standby (:squawking-mode craft))
    (do
      (q/quad 0 craft-radius-minus
              craft-radius-plus 0
              0 craft-radius-plus
              craft-radius-minus 0))
    (= "1200" (:squawking craft))
    (do
      (q/no-fill)
      (q/rect-mode :radius)
      (q/rect 0 0 craft-radius-plus craft-radius-plus))
    :else
    (do 
      (q/text-align :center :center)
      (q/text-size 14)
      (q/text "*" 0 (* -0.18 (q/text-ascent))))))
(defmethod my-aircraft :default [scheme craft]
  (q/ellipse-mode :center)
  (q/ellipse 0 0 craft-width craft-height))

(deftype GroundRadarMode []
  RadarMode
  (draw-aircraft [this radar scheme craft]
    ;; we can simply wrap up our specific artist
    ;;  in the base draw method
    (do-draw-aircraft radar scheme 
                      my-aircraft my-aircraft
                      draw-info-box
                      craft)))

(defn create-mode [] (GroundRadarMode.))
