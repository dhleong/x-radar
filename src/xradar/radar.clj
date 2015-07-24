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
             [mode :as m :refer [RadarMode]]]
            [xradar.modes.xradar-mode :refer [create-mode]]))

;;
;; Constants
;;

(def default-size [800 800])
(def default-location [20 20])
(def fps 10)

(def bar-text-size 14)
(def bar-padding 10)
(def echo-text-size 13.5)

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
        input-mode (:mode input)
        scheme (-> radar :profile :scheme)
        mode (-> radar :profile :mode)
        selected (-> radar :selected)
        aircraft (-> radar :aircraft)]
    (q/background (:background scheme))
    (q/text-align :left)
    (when-let [selected-craft (get aircraft selected nil)]
      (q/text-size bar-text-size)
      (q/text (str (:callsign selected-craft)) 
              bar-padding 
              (- (q/height) bar-padding)))
    (let [radar-state (assoc radar :mode input-mode)]
      (doseq [[cid craft] aircraft]
        (let [updated-craft
              (if (= selected (:cid craft))
                (assoc craft :state :selected)
                craft)]
          (m/draw-aircraft mode radar-state scheme updated-craft))))
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
      (q/text (describe-input (-> state :input)) 10 10)))
  ;; ensure the state is returned (we don't change anything)
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

(defn create-radar [raw-profile network]
  {:pre [(satisfies? XRadarNetwork network)]}
  (let [profile (fill-profile raw-profile)
        state (atom {:profile profile
                     :network network
                     :aircraft {}})]
    (q/defsketch xradar
      :title "xRadar"
      :setup setup
      :setup-params [state]
      :draw draw
      :size (:size profile)
      :features [:resizable]
      :key-pressed on-key-press
      :key-released on-key-release
      :middleware [qm/pause-on-error qm/fun-mode setup-params])
    (position-window xradar (:win-position profile))
    (swap! state #(assoc % :sketch xradar))
    state))

(defn- aircraft [cid x y]
  {:cid cid :x x :y y :callsign (str "ACA26" cid)
   :type "B737/L" 
   :depart "KLGA" :arrive "KBOS" :alternate ""
   :cruise "FL310" :route "MERIT ROBUC3" 
   :scratch "" :squawk ""
   :remarks "/v/" :rules :ifr})

(defn- testing []
  (def radar (create-radar {:debug true}
                           (reify XRadarNetwork
                             (update-flightplan [this aircraft]
                               (def last-action {:update-fp aircraft})))))
  (update-aircraft radar (aircraft 2 50 50))
  (update-aircraft radar (aircraft 3 150 100))
  (update-aircraft radar (aircraft 4 250 100))
  (update-aircraft radar (aircraft 5 150 200))
  (update-aircraft radar (aircraft 6 50 300))
  (update-aircraft radar (aircraft 7 100 200))
  (update-aircraft radar (aircraft 8 300 300))
  "Opened!")
