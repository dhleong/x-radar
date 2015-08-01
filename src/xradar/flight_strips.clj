(ns ^{:author "Daniel Leong"
      :doc "Rendering of flight strips"}
  xradar.flight-strips
  (:require [clojure.string :refer [lower-case split]]
            [clojure.java.io :as io]
            [clj-time.local :as l]
            [quil.core :as q]
            [xradar
             [radar-util :refer [redraw]]
             [util :refer [with-alpha]]]))

(def strip-width 500)
(def strip-height 60)
(def strip-border 2)
(def strip-lines 1)
(def strip-padding 5)
(def half-padding (/ strip-padding 2))
(def text-size 12)

(def max-bays 2)

(defn render-strip
  "Renders the strip at the 'current location.'
  In other words, you must translate as appropriate
  so that the current 0,0 is the desired upper-left
  corner of the strip. The strip should have some
  extra fields not normally associated with an aircraft:
  :flight-type <:departure|:arrival|:local|:vfr|:over>
  :selected <bool>"
  [scheme craft]
  (let [flight-type (get craft :flight-type :unknown)
        background (-> scheme :strips flight-type)
        foreground (-> scheme :strips :foreground)
        char-width (q/text-width "M")
        col1-width (q/text-width "ACA1234B")
        col2-width (q/text-width "FL123 ")
        col2-x (+ col1-width col2-width)
        col3-x (+ col2-x col2-width)
        large-row-height (+ strip-padding text-size)
        route-width (- strip-width 
                       strip-border strip-border
                       strip-padding strip-padding
                       col1-width col2-width col2-width)]
    (if (:selected craft)
      (q/stroke-int (-> scheme :strips :selected-border))
      (q/stroke-int (-> scheme :strips :border)))
    (with-alpha q/fill-int background)
    (q/stroke-weight strip-border)
    (q/rect 0 0 strip-width strip-height)
    (q/stroke-weight strip-lines)
    (q/line col1-width 0 col1-width strip-height)
    (q/line col2-x 0 
            col2-x strip-height)
    (q/line col3-x 0 
            col3-x strip-height)
    (q/with-translation [col1-width (+  strip-padding)]
      (q/line 0 large-row-height col2-width large-row-height)
      (q/line 0 (* 2 large-row-height)
              col2-width (* 2 large-row-height)))
    (q/fill-int foreground)
    (q/text-size text-size)
    ;; column 1
    (q/translate (+ strip-padding strip-border) strip-border)
    (q/text (str (:callsign craft)) 
            0
            large-row-height)
    (q/text (str (:type craft)) 
            0
            (* 2 large-row-height))
    (q/text (apply str (take 3 (str (:cid craft)))) 
            0
            (* 3 large-row-height))
    (q/text (case (:rules craft)
              :ifr "I"
              :vfr "V")
            (- col1-width (* 2 char-width) strip-padding)
            (* 3 large-row-height))
    ;; column 2
    (q/translate col1-width 0)
    (q/text (str (:squawk craft)) 
            0
            large-row-height)
    (q/text (str (:temp-alt craft)) 
            0
            (* 2 large-row-height))
    (q/text (str (:cruise craft)) 
            0
            (* 3 large-row-height))
    ;; column 2
    (q/translate col2-width 0)
    (q/text (str (:depart craft)) 
            0
            (+ half-padding text-size))
    (q/text (str (:arrive craft)) 
            0
            (+ half-padding (* 2 text-size)))
    (q/text (str (:alternate craft)) 
            0
            (+ half-padding (* 3 text-size)))
    (q/text (str (:scratch craft)) 
            0
            (+ half-padding (* 4 text-size)))
    ;; column 3 (route/remarks)
    (q/translate col2-width 0)
    (q/text (str (:route craft))
            0 half-padding
            route-width (* 3 text-size))
    (q/text (str (:remarks craft))
            0 (+ half-padding (* 3 text-size))
            route-width (* 4 text-size))))

(defn render-strip-bay
  "Render the flight strip bay"
  [radar strips]
  ;; TODO
  (let [scheme (-> radar :profile :scheme)
        aircraft (-> radar :aircraft)]
    (when-let [craft (first (vals aircraft))]
      (q/with-translation [10 50]
        (render-strip scheme
                      (assoc craft 
                             ;; :flight-type :vfr
                             :selected false))))))

(defn create-strip-bay
  "Create the datastructure that stores flight strips"
  []
  (atom {:cursor [0 0]
         ;; bays; each is just stored as an index
         0 []
         1 []}))

(defn- move-strip-pred
  [bay old-x old-y new-x new-y]
  (let [strip (get (get bay old-x) old-y)
        old-bay-before (subvec (get bay old-x)
                               0 old-y)
        old-bay-after (subvec (get bay old-x)
                              (inc old-y))
        old-bay (vec (concat old-bay-before old-bay-after))
        with-removed (assoc bay old-x old-bay)
        new-bay-before (subvec (get with-removed new-x)
                               0 new-y)
        new-bay-after (subvec (get with-removed new-x)
                              new-y)
        new-bay (vec (concat new-bay-before [strip] new-bay-after))]
    (assoc with-removed
           new-x new-bay)))

(defn move-current-strip
  "Move the strip under the cursor in the provided
  direction. Has the effect of moving the cursor as well"
  [bay-atom direction]
  (let [bay @bay-atom
        [old-x old-y] (:cursor bay)
        [new-x new-y] (move-strip-cursor bay-atom direction)]
    (when-not (and (= old-x new-x) (= old-y new-y))
      (swap! bay-atom 
             move-strip-pred 
             old-x old-y new-x new-y))))

(defn move-strip-cursor
  "Move the strip cursor in the provided direction.
  Returns the new cursor"
  [bay-atom direction]
  (let [bay @bay-atom
        [old-x old-y] (:cursor bay)
        [mod-x mod-y] (case direction
                        :left [-1 0]
                        :up [0 -1]
                        :right [1 0]
                        :down [0 1])
        new-x (min (- max-bays 1)
                   (max 0 (+ old-x mod-x)))
        new-y (min (- (count (get bay new-x)) 1)
                   (max 0 (+ old-y mod-y)))]
    (when (get (get bay new-x) new-y)
      (swap! bay-atom assoc :cursor [new-x new-y]))
    (:cursor @bay-atom)))
