(ns ^{:author "Daniel Leong"
      :doc "Radar UI module"}
  xradar.radar
  (:require [clojure.string :refer [join]]
            [quil
             [core :as q] 
             [middleware :as qm]]
            [seesaw.core :as s]
            [xradar
             [alias-vars :refer [alias-functions alias-variables]]
             [input :refer [create-input describe-input 
                            process-input-press process-input-release
                            reset-modifiers!]]
             [flight-strips :refer [create-strip-bay render-strip-bay]]
             [lists :refer [create-lists render-lists]]
             [mode :as m :refer [RadarMode]]
             [network :refer [XRadarNetwork]]
             [notif :refer [ack-attention! draw-notifs]]
             [output :refer [create-output-buffers draw-output
                             invalidate-output!]]
             [profile :refer [read-profile]]
             [radar-util :refer [redraw update-aircraft]]
             [schemes :as schemes]
             [scene :refer [XScene get-center get-lon-scale
                            get-magnetic-var loaded? draw-scene]]
             [sector-scene :refer [load-sector parse-coord]]
             [selection-mode :refer [render-selections]]
             [util :refer [deep-merge with-alpha]]
             [weather :refer [draw-weather]]]
            [xradar.modes.ground-mode :refer [create-mode]]))

;;
;; Constants
;;

(def default-location [20 20])
(def default-size [800 800])
(def default-zoom 220)
(def fps 30)
(def renderer :opengl)

(def strips-bay-offset [20 20])

;; NB we disable looping and just redraw
;;  to avoid pegging the CPU. If you don't
;;  care, you can loop for faster redraw
(def dont-loop true)

(def bar-text-size 14)
(def bar-padding 10)
(def echo-text-size 13.5)

;;
;; Util
;;

(defn- fill-profile
  "Ensure some defaults exist in the profile"
  [profile]
  (deep-merge {:arrivals []
               :departures []
               :draw [:geo :labels]
               :mode (create-mode)
               :output-size 5
               :scheme schemes/default
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
  (let [monospaced-12 (q/create-font "Monospaced" 12)
        monospaced-14 (q/create-font "Monospaced" 14)]
    (q/text-font monospaced-14))
  (q/frame-rate fps)
  (q/background-int (get-in @state-atom [:profile :scheme :background]))
  (let [input (create-input (-> @state-atom :profile))]
    ;; add a reference to input in the radar state
    (swap! state-atom #(assoc % :input input))
    {:radar-state state-atom
     :input input}))

(defn draw [state]
  (let [radar-atom (:radar-state state)
        radar @radar-atom
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
    (q/hint :enable-depth-test)
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
                (assoc craft :selected true)
                craft)]
          (m/draw-aircraft mode radar-state scheme updated-craft))))
    ;; reset camera mode for UI
    (q/hint :disable-depth-test)
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
      ;; flight strips mode
      :strips (q/with-translation strips-bay-offset
               (render-strip-bay radar))
      ;; selection mode
      :select (render-selections radar)
      ;; default; do nothing
      nil)
    (when-let [echo (:last-echo input)]
      (q/text-size echo-text-size)
      (q/text-align :left)
      (q/fill-int (-> scheme :echo))
      ;; (q/text nil)
      (q/text (if (string? echo)
               echo
               (join " " echo)) 
              (+ bar-padding (q/text-width "XXX1234") bar-padding)
              (- (q/height) bar-padding )))
    ;; draw output
    (q/with-translation [0 (- (q/height) 
                              bar-padding bar-text-size bar-padding)]
      (let [s (System/currentTimeMillis)]
        (draw-output radar-atom)
        (def duration (- (System/currentTimeMillis) s))))
    ;; top bar stuff
    (q/with-translation [0 0] ;; can be translated as necessary
      (draw-notifs radar)
      (draw-weather radar))
    ;; debugging
    (when (-> radar :profile :debug)
      (q/with-translation [0 30]
        (q/fill-int 0xffFFFFFF)
        (q/text-align :left)
        (q/text-size 11)
        (q/text (describe-input (-> state :input)) 10 10)))
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
        (when dont-loop (q/no-loop))
        (assoc state :loaded true))
      state)))

(defn on-gain-focus [state]
  (q/redraw)
  (ack-attention!)
  (reset-modifiers! (:input state))
  state)

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
        state (atom (deep-merge
                      {:functions alias-functions
                       :history-command (atom [])
                       :history-insert (atom [])
                       :network network
                       :profile profile
                       :output-scroll 0
                       :scene scene
                       :strips (create-strip-bay)
                       :variables alias-variables
                       :aircraft {}}
                      (create-lists)
                      (create-output-buffers)))]
    (q/defsketch xradar
      :title "xRadar"
      :setup setup
      :setup-params [state]
      :draw draw
      :update update
      :renderer renderer
      :size (:size profile)
      :features [:resizable]
      :focus-gained on-gain-focus
      :key-pressed on-key-press
      :key-released on-key-release
      :middleware [qm/pause-on-error qm/fun-mode setup-params])
    (position-window xradar (:win-position profile))
    (swap! state #(assoc % :sketch xradar))
    (s/listen xradar 
              :component-resized
              (fn [_]
                (invalidate-output! state)
                (redraw state)))
    state))

(defn- aircraft [cid lat lon]
  {:cid cid :callsign (str "ACA26" cid)
   :x (parse-coord lon)
   :y (parse-coord lat)
   :type "B737/L" 
   :depart "KLGA" :arrive "KBOS" :alternate ""
   :cruise "FL310" :route "MERIT ROBUC3" 
   :scratch "" :squawk ""
   :squawking (case cid
                2 "2500"
                3 "1200"
                4 "0000"
                "0000")
   :squawking-mode (case cid
                     2 :normal
                     3 :normal
                     4 :standby
                     :standby)
   :tracked-by nil ;; eg "A"
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

(defn- make-controllers
  "For testing only"
  []
  [{:cid 111 :callsign "JFK_GND"}
   {:cid 112 :callsign "JFK_TWR"}
   {:cid 113 :callsign "LGA_TWR"}])

(defn- testing []
  (def radar-connection (atom nil))
  (def radar 
    (create-radar 
      (assoc (read-profile) :debug true)
      (load-sector 
        "/Users/dhleong/VRC/Support/ZNY.sct2"
        #(add-aircraft radar))
      (reify XRadarNetwork
        (config-voice! [this config]
          (future (swap! (:input @radar) #(assoc % :last-echo (str "! " config)))))
        (connected? [this]
          (not (nil? @radar-connection)))
        (connect! [this params]
          (let [{:keys [on-connect]} params]
            (swap! radar-connection (constantly params))
            (on-connect)))
        (disconnect! [this]
          (swap! radar-connection (constantly nil)))
        (get-controllers [this]
          (make-controllers))
        (get-servers [this]
          {"USA-E" {:ip "97.107.135.245"}
           "USA-W" {:ip "50.116.3.203"}})
        (my-callsign [this]
          (:callsign @radar-connection))
        (push-strip! [this cid strip]
          ;; just ignore
          nil)
        (send! [this message]
          ;; use future to avoid deadlock
          (future (swap! (:input @radar) #(assoc % :last-echo (str ">>" message)))))
        (send-to! [this cid message]
          ;; use future to avoid deadlock
          (future (swap! (:input @radar) #(assoc % :last-echo (str ">>" cid ": " message)))))
        (update-flightplan [this aircraft]
          (def last-action {:update-fp aircraft})))))
  "Opened!")
