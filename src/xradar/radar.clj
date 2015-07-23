(ns ^{:author "Daniel Leong"
      :doc "Radar UI module"}
  xradar.radar
  (:require [quil
             [core :as q]
             [applet :refer [applet-close]]
             [middleware :as qm]]
            [xradar
             [input :refer [create-input describe-input 
                            process-input-press process-input-release]]
             [schemes :as schemes]
             [mode :as m :refer [RadarMode]]]
            [xradar.modes.xradar-mode :refer [create-mode]]))

;;
;; Constants
;;

(def default-size [800 800])
(def default-location [20 20])
(def fps 10)

;;
;; Util
;;

(defn- fill-profile
  "Ensure some defaults exist in the profile"
  [profile]
  (merge {:mode (create-mode)
          :scheme schemes/default
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
  ;; (q/smooth)
  (q/frame-rate fps)
  (q/background (get-in @state-atom [:profile :scheme :background]))
  {:radar-state state-atom
   :input (create-input (-> @state-atom :profile))})

(defn draw [state]
  (let [radar @(:radar-state state)
        input @(:input state)
        scheme (-> radar :profile :scheme)
        mode (-> radar :profile :mode)
        selected (-> radar :selected)
        aircraft (-> radar :aircraft)]
    (q/background (:background scheme))
    (if-let [selected-craft (get aircraft selected nil)]
      (q/text (str selected-craft) 10 (- (q/height) 30)))
    (doseq [[cid craft] aircraft]
      (let [updated-craft
            (if (= selected (:cid craft))
              (assoc craft :state :selected)
              craft)]
        (m/draw-aircraft mode scheme updated-craft)))
    (case mode
      ;; insert mode; draw the input buffer
      :insert
      (do
        (q/stroke 0xFFF) ;; TODO color scheme
        (q/text (str (:insert-buffer input))))
      ;; default; do nothing
      nil)
    ;; debugging
    (q/fill-int 0xffFFFFFF)
    (q/text (describe-input (-> state :input)) 10 (- (q/height) 10)))
  ;; ensure the state is returned
  state)

(defn on-key-press [state event]
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

(defn update-aircraft
  [radar craft]
  (swap! 
    radar 
    (fn [radar craft] 
      (assoc-in radar [:aircraft (:cid craft)] craft))
    craft))

(defn destroy-radar [radar]
  (applet-close (:sketch @radar)))

(defn create-radar [raw-profile]
  (let [profile (fill-profile raw-profile)
        state (atom {:profile profile
                     :aircraft {}})]
    (q/defsketch xradar
      :title "xRadar"
      :setup setup
      :setup-params [state]
      :draw draw
      :size (:size profile)
      :features [:resizeable]
      :key-pressed on-key-press
      :key-released on-key-release
      :middleware [qm/fun-mode qm/pause-on-error setup-params])
    (position-window xradar (:win-position profile))
    (swap! state #(assoc % :sketch xradar))
    state))

(defn- aircraft [cid x y]
  {:cid cid :x x :y y :callsign "ACA263"})

(defn- testing []
  (def radar (create-radar {}))
  (update-aircraft radar (aircraft 2 50 50)))
