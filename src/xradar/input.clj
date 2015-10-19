(ns ^{:author "Daniel Leong"
      :doc "Modal input state machine"}
  xradar.input
  (:require [clojure.string :refer [join lower-case]]
            [quil
             [applet :refer [applet-close]]]
            [xradar
             [bindings :refer [read-default-bindings]]
             [commands :as c]
             [util :refer [deep-merge]]])
  (:import [java.awt.event KeyEvent]))

(defn- stop-insert-if-necessary
  [machine state]
  (def stop-insert? (:last-press machine))
  (if (> (count (:current-sequence machine)) 1)
    (c/stop-insert machine state)
    (assoc machine :current-sequence [])))

(defmacro exec-press [& body]
  (let [body-full
        (if (even? (count body)) 
          (concat body ['stop-insert-if-necessary])
          body)]
    `(if-let [handler# 
              (try
                (condp = (:key (:last-press ~'machine))
                  ~@body-full)
                (catch IllegalArgumentException e#
                  nil))]
       ;; handler!
       (handler# ~'machine ~'state)
       ;; not handled
       ~'machine)))

(defmulti pressed-in-mode (fn [machine state] (:mode machine)))
(defmethod pressed-in-mode :normal
  [machine state]
  (exec-press
    ;; NB: clojure doesn't like us cross-referencing
    ;;  the destroy-radar method...
    :cmd-w (applet-close (:sketch @state))))
;
(defmethod pressed-in-mode :default
  [machine state]
  (def called-default (:last-press machine))
  (exec-press
    :esc c/stop-insert))

(defn translate-event
  "Given a key event and the current machine state,
  return an translated event whose :key value will
  map exactly to a key mapping form (eg: :alt-L if
  `l` is pressed while `alt` and `shift` are also
  pressed)"
  [machine event]
  (let [key-raw (case (:key-code event)
                  27 "ESC"
                  9 "TAB"
                  16 "shift"
                  32 "SPACE"
                  45 (str (:raw-key event))
                  47 "SLASH"
                  59 "colon" ;; NB it will always be shifted, so...
                  61 (str (:raw-key event))
                  ;; default; fetch
                  (KeyEvent/getKeyText (:key-code event)))
        modifiers (:modifiers machine)
        shifted (contains? modifiers :shift)
        key-label (if shifted key-raw (lower-case key-raw))
        mods (->> modifiers
                  (filter #(not= % :shift))
                  sort
                  (map name)
                  (join "-"))
        key-name-parts (->> [mods key-label]
                            (remove empty?))]
    (assoc event :key (keyword (join "-" key-name-parts)))))

(defn- follow-key-branch
  [machine state branch]
  (let [call (:call branch)
        has-others (seq (->> branch keys (remove #(= % :call))))]
    (cond 
      ;; a callable and no sub keys
      (and call (not has-others))
      (c/eval-command machine state call)
      ;; a callable but has sub keys
      (and call has-others)
      machine ; FIXME we just do nothing for now
      ;; other keys under this
      (seq has-others)
      (assoc machine :current-bindings branch)
      ;; nothing else to do; revert to normal mode
      :else (c/stop-insert machine state))))

(defn- add-modifier
  [machine modifier]
  (assoc machine 
         :modifiers
         (conj (:modifiers machine) modifier)))

(defn- remove-modifier
  [machine modifier]
  (assoc machine 
         :modifiers
         (disj (:modifiers machine) modifier)))

(defn process-press
  "Process keypress. Returns the new value of the input machine"
  [machine state event]
  (case (:key event)
    :shift (add-modifier machine :shift)
    :alt (add-modifier machine :alt)
    :command (add-modifier machine :cmd)
    :control (add-modifier machine :ctrl)
    ;; default
    ;; NB: last echo is cleared on any keypress!
    (let [new-machine (assoc machine :last-echo nil)
          modded-event (translate-event new-machine event)
          current-bindings (-> new-machine :current-bindings)
          the-key (:key modded-event)
          current-sequence (conj 
                             (or (:current-sequence new-machine) [])
                             the-key)
          sequenced-machine (assoc new-machine 
                                   :current-sequence current-sequence
                                   :last-press modded-event)]
      (if-let [branch (get current-bindings the-key)]
        (follow-key-branch sequenced-machine state branch)
        (pressed-in-mode sequenced-machine state)))))

(defn process-release
  "Process key release. Returns the new value of the input machine"
  [machine state event]
  (let [modded-event (translate-event machine event)
        the-key (:key modded-event)]
    (case the-key
      :shift (remove-modifier machine :shift)
      :alt (remove-modifier machine :alt)
      :command (remove-modifier machine :cmd)
      :control (remove-modifier machine :ctrl)
      ;; default
      (let [current-bindings (:current-bindings machine)
            branch (get current-bindings the-key)]
        (when (= 'transmit-voice (:call branch))
          (c/eval-command machine state 'stop-transmit-voice))
        machine))))


;;
;; Public interface
;;

(defn describe-input
  [machine-atom]
  ;; TODO
  (let [machine @machine-atom]
    (str (select-keys machine [:current-bindings :mode :selected :last-press]))))

(defn process-input-press
  "Process key pressed and update the machine"
  [machine-atom state event]
  (swap! machine-atom process-press state event))

(defn process-input-release
  "Process key released and update the machine"
  [machine-atom state event]
  (swap! machine-atom process-release state event))

(defn reset-modifiers!
  "Clear the current set of modifiers"
  [machine-atom]
  (swap! machine-atom 
         assoc 
         :modifiers #{}
         :current-sequence []))

(defn create-input 
  "Prepare a new input machine, given a profile map."
  [profile]
  (let [defaults (read-default-bindings)
        merged-bindings (deep-merge
                          (:bindings defaults)
                          (:bindings profile))]
    (atom {:mode :normal
           :modifiers #{}
           :insert-buffer []
           :bindings merged-bindings
           :current-bindings (:normal merged-bindings)
           :settings (deep-merge
                       (:settings defaults)
                       (:settings profile))})))
