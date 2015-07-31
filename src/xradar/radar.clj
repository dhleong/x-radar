(ns ^{:author "Daniel Leong"
      :doc "Radar UI module"}
  xradar.radar
  (:require [clojure.string :refer [join]]
            [quil
             [core :as q] 
             [middleware :as qm]]
            [xradar
             [commands :refer [use-native-input]]
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

(def default-location [20 20])
(def default-size [800 800])
(def default-zoom 220)
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
               :output-size 5
               :smoothing 3
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
  (let [input (create-input (-> @state-atom :profile))]
    ;; add a reference to input in the radar state
    (swap! state-atom #(assoc % :input input))
    {:radar-state state-atom
     :input input}))

(defn draw [state]
  (let [radar @(:radar-state state)
        input @(:input state)
        input-mode (:mode input)
        {:keys [profile scene selected
                aircraft camera zoom]} radar
        {:keys [scheme mode]} profile
        this-camera (or camera (get-center scene))
        this-zoom (or zoom default-zoom)
        scene-loaded (loaded? scene)
        just-loaded (and (not (:loaded state)) scene-loaded)]
    (q/background-int (:background scheme))
    (q/text-mode :shape)
    (when scene-loaded
      (let [{:keys [x y]} this-camera]
        (q/begin-camera)
        (q/camera x y this-zoom
                  x y 0
                  0 1 0)
        (q/end-camera))
      (draw-scene scene profile))
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
      (when-not use-native-input
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
                 (+ b (q/text-descent)))))
      ;; default; do nothing
      nil)
    (when-let [echo (:last-echo input)]
      (q/text-size echo-text-size)
      (q/text-align :left)
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
    state))

(defn update [state]
  (let [radar @(:radar-state state)
        {:keys [scene camera zoom]} radar
        this-camera (or camera (get-center scene))
        this-zoom (or zoom default-zoom)
        scene-loaded (loaded? scene)
        just-loaded (and (not (:loaded state)) scene-loaded)]
    (if just-loaded
      (do
        (swap! (:radar-state state)
               #(assoc %
                       :zoom this-zoom
                       :camera this-camera))
        (q/no-loop)
        (assoc state :loaded true))
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
                     :output-buffer (atom [])
                     :scene scene
                     :aircraft {}})]
    (q/defsketch xradar
      :title "xRadar"
      :setup setup
      :setup-params [state]
      :draw draw
      :update update
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

(defn- add-aircraft
  "For testing only"
  [radar]
  (update-aircraft radar (aircraft 2 "N040.46.18.052" "W073.51.27.664"))
  (update-aircraft radar (aircraft 3 "N040.46.29.321" "W073.52.04.109"))
  (update-aircraft radar (aircraft 4 "N040.46.38.053" "W073.52.21.403"))
  #_(update-aircraft radar (aircraft 6 50 300))
  #_(update-aircraft radar (aircraft 7 100 200))
  #_(update-aircraft radar (aircraft 8 300 300)))

(defn- testing []
  (def radar 
    (create-radar 
      {:debug true}
      (load-sector 
        "/Users/dhleong/VRC/Support/ZNY.sct2"
        #(add-aircraft radar))
        (reify XRadarNetwork
          (send! [this message]
            ;; use future to avoid deadlock
            (future (swap! (:input @radar) #(assoc % :last-echo (str ">>" message)))))
          (send-to! [this cid message]
            ;; use future to avoid deadlock
            (future (swap! (:input @radar) #(assoc % :last-echo (str ">>" cid ": " message)))))
          (update-flightplan [this aircraft]
            (def last-action {:update-fp aircraft})))))
  "Opened!")
