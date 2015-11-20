(ns ^{:author "Daniel Leong"
      :doc "Utilities"}
  xradar.util
  (:require [clojure.string :refer [upper-case]]
            [clojure.core.matrix :refer [matrix inner-product set-current-implementation]]
            [clojure.java.io :refer [file]]
            [quil.core :as q]
            [seesaw
             [bind :as b]
             [core :as s]]
            [xradar
             [network :refer [get-controllers]]
             [scene :refer [find-point get-center get-lon-scale loaded?]]]))

(set-current-implementation :vectorz) 

(def coord-scale 10000)
(def earth-radius-nm 3440)

(defn- cid-to-controller
  [radar cid]
  (let [network (:network radar)
        controllers (get-controllers network)]
    (->> controllers
        (filter #(or (= cid (:cid %))
                     (= cid (:callsign %))))
        first)))

(defn bearing-to
  "Calculate the bearing from one point to another"
  ([p1 p2]
   (bearing-to (:y p1) (:x p1) (:y p2) (:x p2)))
  ([lat1 lon1 lat2 lon2]
   (Math/atan2 
     (- lat2 lat1)
     (- lon2 lon1))))

(defn distance-between
  "Calculate the distance between two scaled coordinates
  in nautical miles"
  ([p1 p2]
   (let [lat1 (/ (:y p1) coord-scale)
         lon1 (/ (:x p1) coord-scale)
         lat2 (/ (:y p2) coord-scale)
         lon2 (/ (:x p2) coord-scale)]
     (distance-between lat1 lon1 lat2 lon2)))
  ([lat1 lon1 lat2 lon2]
   ;; adapted from: http://www.movable-type.co.uk/scripts/latlong.html
   (let [ph1 (Math/toRadians lat1)
         ph2 (Math/toRadians lat2)
         del-ph (Math/toRadians (- lat2 lat1))
         del-lm (Math/toRadians (- lon2 lon1))
         a (+ (* (Math/sin (/ del-ph 2))
                 (Math/sin (/ del-ph 2)))
              (* (Math/cos ph1)
                 (Math/cos ph2)
                 (Math/sin (/ del-lm 2))
                 (Math/sin (/ del-lm 2))))
         c (* 2 (Math/atan2 (Math/sqrt a), (Math/sqrt (- 1 a))))]
     (* earth-radius-nm c))))

(defn in-focus?
  "Returns true if any xRadar window is in focus."
  []
  (some true? (map #(.isActive %) (s/all-frames))))

(defn set-toggle
  "Toggle the presence of the value
  in the given set. In other words,
  if the-set contains value, we return
  the-set without value contained; otherwise,
  we return the-set WITH the value contained."
  [the-set value]
  (if (contains? the-set value)
    (disj the-set value)
    (conj the-set value)))

(defn when-all-set-enabled
  "When all views with provided ids match
  a given predicate, set the target widget enabled"
  [w pred ids]
  (let [frame (s/to-root w)]
    (b/bind 
      (apply b/funnel 
             (->> ids
                  (map 
                    #(keyword (str "#" (name %))))
                  (map
                    #(s/select frame [%]))))
      (b/transform #(every? pred %))
      (b/property w :enabled?))))

(defn when-none-empty-set-enabled
  "Shortcut to perform when-all-set-enabled
  with the predicate (complement empty?)"
  [w ids]
  (when-all-set-enabled w (complement empty?) ids))

(defn with-alpha
  "Call the given color predicate, providing an alpha
  parameter if it was packed into the int as 0xARGB"
  [pred color-int]
  (let [alpha (bit-shift-right color-int 24)]
    (if (zero? alpha)
      (pred color-int)
      (pred color-int alpha))))

(defn deep-merge [base-map new-map]
  (if (nil? new-map)
    ;; just use the base
    base-map
    ;; do the merge
    (merge-with 
      (fn [x y]
        (cond (and (map? x) (map? x)) (deep-merge x y)
              ;; why did I merge vectors? 
              ;; (and (vector? x) (vector? x)) (concat x y) 
              :else y)) 
      base-map new-map)))

(defn in-bounds
  "Check if a coordinate is within the bounds
  of the currently visible area. Only works
  inside sketch functions"
  ([x y]
   (let [w (q/width)
         h (q/height)
         sx (q/screen-x x y)
         sy (q/screen-y x y)]
     (and (<= 0 sx w)
          (<= 0 sy h))))
  ([rect]
   (let [[rl rt rr rb] rect
         w (q/width)
         h (q/height)
         l (q/screen-x rl rt)
         t (q/screen-y rl rt)
         r (q/screen-x rr rb)
         b (q/screen-y rr rb)]
     ;; rects overlap if:
     ;;  a.l <= b.r AND
     ;;  a.r >= b.l AND
     ;;  a.t <= b.b AND
     ;;  a.b >= b.t
     (and (<= l w)
          (>= r 0)
          (<= t h)
          (>= b 0)))))

(defn list-replace
  "Replace `old-value` with `new-value` in `coll`"
  [old-value new-value coll]
  ;; is there a better way to do this...?
  (map
    #(if (= % old-value)
       new-value
       %)
    coll))

(defn object-for
  "Returns the pilot/controller/waypoint object 
  of the given cid. If it's already an object,
  we simply pass it through"
  [state cid]
  (if (map? cid)
    ;; we already have the object
    cid
    ;; need to find it
    (let [radar (if (map? state)
                  state
                  @state)]
      (get-in radar
              [:aircraft cid]
              ;; try controller or waypoint
              (or (cid-to-controller radar cid)
                  (when-let [scene (:scene radar)]
                    (find-point scene cid)))))))

(defn resolve-id
  "Clean up a user-inputted ID, possibly a CID,
  a callsign for a pilot or controller, or a waypoint.
  Returns nil if the id didn't appear to refer to anything."
  [state id]
  (if (number? id)
    (when (object-for state id)
      id)
    (let [clean (upper-case id)]
      (if (object-for state clean)
        clean
        ;; iterate over aircraft callsigns
        (->> (:aircraft @state)
             vals
             (filter #(= clean (:callsign %)))
             first
             :cid)))))

(defn resolve-obj
  "Given some object-like input, which is either an
  actual object or an arg accepted by resolve-id, 
  return the full object."
  [state obj-or-id]
  (if (map? obj-or-id)
    obj-or-id
    (if-let [obj (object-for state obj-or-id)]
      obj
      (when-let [id (resolve-id state obj-or-id)]
        (object-for state id)))))

(defn resolve-file
  [path]
  (when path
    (let [home (System/getProperty "user.home")]
      (-> path
          (.replace "~" home)
          (.replace "$HOME" home)
          (.replace "%USERPROFILE%" home)
          file))))

;;
;; Wacky hacks
;;

(defn- my-get-center
  [scene-or-data]
  (if (map? scene-or-data)
    (-> scene-or-data :info :center)
    (get-center scene-or-data)))

(defn- my-get-lon-scale
  [scene-or-data]
  (if (map? scene-or-data)
    (if-let [info (:info scene-or-data)]
      (/ (:nm-per-lon info) (:nm-per-lat info))
      1)
    (get-lon-scale scene-or-data)))

(def cached-mat nil)
(defn- get-matrix
  [scene]
  (if-let [cached cached-mat]
    cached
    (when-let [point (my-get-center scene)]
      (when-let [sx (* coord-scale (my-get-lon-scale scene))]
        (let [px (:x point)
              py (:y point)
              sy coord-scale ;; just one (scaled)
              mat (matrix
                    [[sx 0  (- px (* sx px))]
                     [0  sy (- py (* sy py))]
                     [0  0  1]])]
          (def cached-mat mat)
          mat)))))

(defn map-coord
  "Used inside drawing functions to map a
  {:x, :y} coord as appropriate"
  [scene coord]
  (if (and
        scene
        (:x coord)
        (or (map? scene) (loaded? scene)))
    ;; loaded!
    {:x (* (:x coord) coord-scale (my-get-lon-scale scene))
     :y (* (:y coord) coord-scale)}
    #_(if-let [mat (get-matrix scene)]
      (let [mapped 
            (inner-product
              mat
              [(:x coord) (:y coord) 1])]
        {:x (first mapped) :y (second mapped)})
      ;; no matrix, probably tests; just pass through
      coord)
    ;; not loaded; pass through
    coord))
