(ns ^{:author "Daniel Leong"
      :doc "Radar UI module"}
  xradar.radar
  (:require [clojure.string :refer [join]]
            [quil
             [core :as q] 
             [middleware :as qm]]
            [xradar
             [input :refer [create-input describe-input 
                            process-input-press process-input-release]]
             [network :refer [XRadarNetwork]]
             [radar-util :refer [update-aircraft]]
             [schemes :as schemes]
             [scene :refer [XScene get-center get-lon-scale
                            get-magnetic-var loaded? draw-scene]]
             [sector-scene :refer [load-sector parse-coord]]
             [mode :as m :refer [RadarMode]]
             [util :refer [deep-merge]]]
            [xradar.modes.xradar-mode :refer [create-mode]]))

;;
;; Constants
;;

(def default-size [800 800])
(def default-location [20 20])
(def fps 7)
(def renderer :opengl)

(def bar-text-size 14)
(def bar-padding 10)
(def echo-text-size 13.5)

;;
;; Util
;;

(defn- fill-profile
  "Ensure some defaults exist in the profile"
  [profile]
  (deep-merge {:mode (create-mode)
               :scheme schemes/default
               :draw [:geo :labels]
               :smoothing 4
               :size default-size
               :timeout-len 1000 ;; time before a key sequence is dropped
               :win-position default-location}
              profile))

(defn- fix-esc
  [event]
  (if (= 27 (:key-code event))
    (assoc event :key :esc)
    event))

(defn- position-window
  [sketch position]
  (let [[x y] position
        win (loop [s sketch]
              (if (nil? (.getParent s))
                s
                (recur (.getParent s))))]
    (.setLocation win x y)))

(defn- setup-params
  "Middleware that lets you provide params to
  your :setup function using the :setup-params option"
  [options]
  (let [setup-fn (:setup options (fn [& _] nil))
        params (:setup-params options [])]
    (assoc
      options
      :setup
      #(apply setup-fn params))))

;;
;; Quil functions
;;

(defn setup [state-atom]
  (q/smooth (get-in @state-atom [:profile :smoothing] 4))
  (q/frame-rate fps)
  (q/background-int (get-in @state-atom [:profile :scheme :background]))
  {:radar-state state-atom
   :input (create-input (-> @state-atom :profile))})

(defn draw [state]
  (let [radar @(:radar-state state)
        input @(:input state)
        input-mode (:mode input)
        profile (-> radar :profile)
        scheme (-> profile :scheme)
        scene (-> radar :scene)
        mode (-> profile :mode)
        selected (-> radar :selected)
        aircraft (-> radar :aircraft)
        scene-loaded (loaded? scene)
        just-loaded (and (not (:loaded scene)) scene-loaded)]
    (q/background-int (:background scheme))
    (when scene-loaded
      #_(let [{:keys [x y]} (get-center scene)
            scale 70]
        #_(q/text (str x "," y) 20 20)
        (q/translate (- x) (- y)))
      (let [{:keys [x y]} (get-center scene)
            scale 220]
        (q/text (str x "," y) 20 20)
        (q/begin-camera)
        ;; TODO navigate; be more sane?
        (q/camera x y scale
                  x y 0
                  0 1 0)
        #_(q/rotate (/ (q/radians (get-magnetic-var scene)) 100))
        #_(q/push-matrix)
        #_(let [px x
              py y
              sx (get-lon-scale scene)
              sy 1]
          ;; scale, centered on a point.
          ;; the built-in scale always goes
          ;;  from the origin
          #_(q/apply-matrix
            sx 0 0 (- px (* sx px))
            0 sy 0 (- py (* sy py))
            0 0 1 0
            0 0 0 1))
        (q/end-camera))
      ;; TODO is there any way to reduce CPU load?
      (draw-scene scene profile)
      #_(q/pop-matrix))
    (if (not scene-loaded)
      (q/text "Loading..." 20 20))
    (let [radar-state (assoc radar :mode input-mode)]
      (doseq [[cid craft] aircraft]
        (let [updated-craft
              (if (= selected (:cid craft))
                (assoc craft :state :selected)
                craft)]
          (m/draw-aircraft mode radar-state scheme updated-craft))))
    ;; reset camera mode for UI
    (q/camera)
    (when-let [selected-craft (get aircraft selected nil)]
      ;; draw selected aircraft
      (q/text-align :left)
      (q/fill-int (-> scheme :input :text))
      (q/text-size bar-text-size)
      (q/text (str (:callsign selected-craft)) 
              bar-padding 
              (- (q/height) bar-padding)))
    (case input-mode
      ;; insert mode; draw the input buffer
      :insert
      (let [l (+ bar-padding (q/text-width "XXX1234") bar-padding)
            b (- (q/height) bar-padding)]
        ;; draw the text
        (q/text-size bar-text-size)
        (q/fill-int (-> scheme :input :text))
        (q/text (apply str (:insert-buffer input)) l b)
        ;; draw the box
        (q/stroke-int (-> scheme :input :box))
        (q/rect-mode :corners)
        (q/no-fill)
        (q/rect (- l (/ bar-padding 2)) 
                (- (q/height) bar-text-size bar-padding) 
                (- (q/width) bar-padding)
                (+ b (q/text-descent))))
      ;; default; do nothing
      nil)
    (when-let [echo (:last-echo input)]
      (q/text-size echo-text-size)
      (q/fill-int (-> scheme :echo))
      ;; (q/text nil)
      (q/text (join " " echo) 
              bar-padding
              (- (q/height) bar-padding bar-text-size bar-padding)))
    ;; debugging
    (when (-> radar :profile :debug)
      (q/fill-int 0xffFFFFFF)
      (q/text-align :left)
      (q/text-size 11)
      (q/text (describe-input (-> state :input)) 10 10))
    ;; ensure the state is returned
    (if just-loaded
      ;; update our state
      (assoc state :loaded true)
      state)))

(defn on-key-press [state event]
  (q/redraw)
  (process-input-press (:input state) (fix-esc event) (:radar-state state))
  (when (= 27 (q/key-code)) ; 27 is escape key.
    ; Preventing esc from closing the sketch by setting current key to 0.
    (set! (.key (quil.applet/current-applet)) (char 0)))
  state)

(defn on-key-release [state]
  (process-input-release (:input state) (fix-esc {:key (q/key-as-keyword) :key-code (q/key-code)}))
  state)


;;
;; Public interface
;;

(defn create-radar [raw-profile scene network]
  {:pre [(satisfies? XRadarNetwork network)
         (satisfies? XScene scene)]}
  (let [profile (fill-profile raw-profile)
        state (atom {:profile profile
                     :network network
                     :scene scene
                     :aircraft {}})]
    (q/defsketch xradar
      :title "xRadar"
      :setup setup
      :setup-params [state]
      :draw draw
      :renderer renderer
      :size (:size profile)
      :features [:resizable]
      :key-pressed on-key-press
      :key-released on-key-release
      :middleware [qm/pause-on-error qm/fun-mode setup-params])
    (position-window xradar (:win-position profile))
    (swap! state #(assoc % :sketch xradar))
    state))

(defn- aircraft [cid lat lon]
  {:cid cid :callsign (str "ACA26" cid)
   :x (parse-coord lon)
   :y (parse-coord lat)
   :type "B737/L" 
   :depart "KLGA" :arrive "KBOS" :alternate ""
   :cruise "FL310" :route "MERIT ROBUC3" 
   :scratch "" :squawk ""
   :remarks "/v/" :rules :ifr})

(defn- testing []
  (def radar (create-radar {:debug true}
                           (load-sector "/Users/dhleong/VRC/Support/ZNY.sct2")
                           (reify XRadarNetwork
                             (update-flightplan [this aircraft]
                               (def last-action {:update-fp aircraft})))))
  ;; (update-aircraft radar (aircraft 2 50 50))
  (update-aircraft radar (aircraft 2 "N040.46.18.052" "W073.51.27.664"))
  (update-aircraft radar (aircraft 3 "N040.46.29.321" "W073.52.04.109"))
  (update-aircraft radar (aircraft 4 "N040.46.38.053" "W073.52.21.403"))
  #_(update-aircraft radar (aircraft 6 50 300))
  #_(update-aircraft radar (aircraft 7 100 200))
  #_(update-aircraft radar (aircraft 8 300 300))
  "Opened!")
