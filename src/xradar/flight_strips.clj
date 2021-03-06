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
(def separator-text-size 17)

(def max-bays 2)

(defn resolve-flight-type
  [profile craft]
  (let [depart (:depart craft)
        arrive (:arrive craft)
        arrive-fields (:arrivals profile)
        depart-fields (:departures profile)
        arriving? (contains? arrive-fields arrive)
        departing? (contains? depart-fields depart)]
    (cond 
      ;; local must be arriving and departing at the 
      ;;  same airport, one that we control
      (and (= depart arrive)
           (or arriving? departing?))
      :local
      ;; vfr is only when departing
      (and (= :vfr (:rules craft))
           departing?) 
      :vfr
      ;; easy ones
      (empty? (:route craft)) :noplan
      arriving? :arrival
      departing? :departure
      ;; if either of these are empty at this point,
      ;;   we have no idea what it is
      (or (empty? arrive-fields) (empty? depart-fields))
      :unknown
      ;; otherwise, must be an over flight
      :else :over)))

(defn render-strip
  "Renders the strip at the 'current location.'
  In other words, you must translate as appropriate
  so that the current 0,0 is the desired upper-left
  corner of the strip. The strip should have some
  extra fields not normally associated with an aircraft:
  :flight-type <:departure|:arrival|:local|:vfr|:over>
  :selected <bool>"
  [scheme craft]
  (q/text-size text-size)
  (q/text-align :left)
  (let [flight-type (get craft :flight-type :unknown)
        background (-> scheme :strips flight-type)
        foreground (-> scheme :strips :foreground)
        char-width (q/text-width "M")
        col1-width (q/text-width "ACA1234B")
        col2-width (q/text-width " FL123 ")
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
    (q/rect-mode :corner)
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
    ;; column 3
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
    ;; column 4 (route/remarks)
    (q/translate col2-width 0)
    (q/text (str (:route craft))
            0 half-padding
            route-width (* 3 text-size))
    (q/text (str (:remarks craft))
            0 (+ half-padding (* 3 text-size))
            route-width (* 4 text-size))))

(defn render-strip-separator
  [scheme label selected?]
  (if selected?
    (q/stroke-int (-> scheme :strips :selected-border))
    (q/stroke-int (-> scheme :strips :border)))
  (with-alpha q/fill-int (-> scheme :strips :separator :background))
  (q/stroke-weight strip-border)
  (q/rect-mode :corner)
  (q/rect 0 0 strip-width strip-height)
  (q/text-size separator-text-size)
  (q/text-align :center)
  (q/fill-int (-> scheme :strips :separator :text))
  (q/text label (/ strip-width 2) (/ strip-height 2)))

(defn render-strip-bay
  "Render the flight strip bay"
  [radar]
  (let [profile (-> radar :profile)
        scheme (-> profile :scheme)
        aircraft (-> radar :aircraft)
        strips @(-> radar :strips)
        [cursor-x cursor-y] (-> strips :cursor)
        translation-x (* (inc cursor-x) strip-width)
        delta (- (q/width) translation-x)
        translate-vec (if (< delta 0)
                        ;; if rendering it in the normal place
                        ;;  would put it off screen, translate
                        ;;  it to be in a nice place
                        [delta 0]
                        ;; otherwise, do nothing
                        [0 0])]
    (q/with-translation translate-vec
      ;; theres a bit of hax here, but that's okay...
      (doseq [x (range max-bays)
              y (range (max (count (get strips 0)) (count (get strips 1))))]
        (when-let [cid (get (get strips x) y)]
          (let [craft (get aircraft cid)
                selected (and (= x cursor-x)
                              (= y cursor-y))]
            (q/with-translation [(* x strip-width) (* y strip-height)]
              (cond 
                craft
                (render-strip
                  scheme
                  (assoc craft 
                         :flight-type 
                         (resolve-flight-type 
                           profile
                           craft)
                         :selected selected))
                ;; is it a separator?
                (string? cid)
                (render-strip-separator
                  scheme
                  cid
                  selected)))))))))

(defn create-strip-bay
  "Create the datastructure that stores flight strips"
  []
  (atom {:cursor [0 0]
         ;; bays; each is just stored as an index
         0 []
         1 []}))

(defn bays-empty?
  [bay-atom]
  (let [bays @bay-atom]
    (and (empty? (get bays 0))
         (empty? (get bays 1)))))

(defn- move-strip-pred
  [bay old-x old-y & [new-x new-y]]
  (let [strip (get (get bay old-x) old-y)
        old-bay-before (subvec (get bay old-x)
                               0 old-y)
        old-bay-after (subvec (get bay old-x)
                              (inc old-y))
        old-bay (vec (concat old-bay-before old-bay-after))
        with-removed (assoc bay old-x old-bay)]
    (if (nil? new-x)
      ;; easy; we were just removing
      with-removed
      ;; we were moving...
      (let [new-bay-before (subvec (get with-removed new-x)
                                   0 new-y)
            new-bay-after (subvec (get with-removed new-x)
                                  new-y)
            new-bay (vec (concat new-bay-before [strip] new-bay-after))]
        (assoc with-removed
               new-x new-bay)))))


(defn move-strip-cursor
  "Move the strip cursor in the provided direction.
  If force-move is true, we will be allowed to move
  into empty bays/columns.
  Returns the new cursor"
  [bay-atom direction & {:keys [force-move]}]
  (let [bay @bay-atom
        [old-x old-y] (:cursor bay)
        [mod-x mod-y] (case direction
                        :left [-1 0]
                        :up [0 -1]
                        :right [1 0]
                        :down [0 1])
        new-x (min (- max-bays 1)
                   (max 0 (+ old-x mod-x)))
        new-y (max 0 (min (- (count (get bay new-x)) 1)
                          (+ old-y mod-y)))]
    (when (or (and force-move 
                   (< -1 new-x max-bays)
                   (>= new-y 0)) 
              (get (get bay new-x) new-y))
      (swap! bay-atom assoc :cursor [new-x new-y]))
    (:cursor @bay-atom)))

(defn add-strip
  "Add a strip for the given client id"
  [bay-atom cid]
  (swap! bay-atom 
         #(assoc % 0 (vec (cons cid (get % 0))))))

(defn add-separator
  "Add a separator with the given label"
  [bay-atom label]
  ;; NB it's the same thing, really
  (add-strip bay-atom label))

(defn delete-current-strip
  [bay-atom]
  (let [bay @bay-atom
        [old-x old-y] (:cursor bay)
        _ (move-strip-cursor bay-atom :up)]
    (swap! bay-atom 
           move-strip-pred 
           old-x old-y)))

(defn get-current-strip
  "Returns the CID associated with the
  currently selected flight strip"
  [bay-atom]
  (let [bay @bay-atom
        [x y] (:cursor bay) ]
    (get (bay x) y)))

(defn move-current-strip
  "Move the strip under the cursor in the provided
  direction. Has the effect of moving the cursor as well"
  [bay-atom direction]
  (let [bay @bay-atom
        [old-x old-y] (:cursor bay)
        [new-x new-y] (move-strip-cursor bay-atom direction 
                                         :force-move true)]
    (when-not (and (= old-x new-x) (= old-y new-y))
      (swap! bay-atom 
             move-strip-pred 
             old-x old-y new-x new-y))))
