(ns ^{:author "Daniel Leong"
      :doc "Modal input state machine"}
  xradar.input
  (:require [xradar
             [bindings :refer [default-bindings]]
             [commands :as c]
             [util :refer [deep-merge]]]))

(defn- match-key [matcher event]
  (if (keyword? matcher)
    (and (= matcher (:key event))
         (empty? (:modifiers event)))
    (let [key-pressed (last matcher)
          mods (apply hash-set (drop-last matcher))]
      (and (= key-pressed (:key event))
           (= mods (:modifiers event))))))

(defmacro exec-press [& body]
  `(if-let [handler# 
            (try
              (condp match-key (:last-press ~'machine)
                ~@body)
              (catch IllegalArgumentException e#
                nil))]
     ;; handler!
     (handler# ~'machine ~'state)
     ;; not handled
     ~'machine))

(defmulti pressed-in-mode (fn [machine state] (:mode machine)))
(defmethod pressed-in-mode :insert
  [machine state]
  (exec-press
    :esc c/stop-insert
    ;; default handler
    c/handle-insert))
;
(defmethod pressed-in-mode :normal
  [machine state]
  (exec-press
    :i c/start-insert
    :s c/start-select-aircraft))
;
(defmethod pressed-in-mode :default
  [machine state]
  (exec-press
    :esc c/stop-insert
    ;; just return the machine
    machine))


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

(defn- process-press
  "Process keypress. Returns the new value of the input machine"
  [machine event state]
  (case (:key event)
    :shift (add-modifier machine :shift)
    :alt (add-modifier machine :alt)
    :command (add-modifier machine :command)
    :control (add-modifier machine :control)
    ;; default
    (let [modded-event (assoc event :modifiers (:modifiers machine))]
      (pressed-in-mode (assoc machine :last-press modded-event)
                       state))))

(defn- process-release
  "Process key release. Returns the new value of the input machine"
  [machine event]
  (case (:key event)
    :shift (remove-modifier machine :shift)
    :alt (remove-modifier machine :alt)
    :command (remove-modifier machine :command)
    :control (remove-modifier machine :control)
    ;; default
    machine))


;;
;; Public interface
;;

(defn describe-input
  [machine-atom]
  ;; TODO
  (let [machine @machine-atom]
    (str machine)))

(defn process-input-press
  "Process key pressed and update the machine"
  [machine-atom event state]
  (swap! machine-atom process-press event state))

(defn process-input-release
  "Process key released and update the machine"
  [machine-atom event]
  (swap! machine-atom process-release event))


(defn create-input 
  "Prepare a new input machine, given a profile map."
  [profile]
  (atom {:mode :normal
         :modifiers #{}
         :insert-buffer []
         :bindings (deep-merge
                     default-bindings
                     (:bindings profile))}))
