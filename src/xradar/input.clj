(ns ^{:author "Daniel Leong"
      :doc "Modal input state machine"}
  xradar.input
  (:require [clojure.string :refer [join lower-case]]
            [xradar
             [bindings :refer [read-default-bindings]]
             [commands :as c]
             [util :refer [deep-merge]]])
  (:import [java.awt.event KeyEvent]))

(defn- match-key [matcher event]
  (if (keyword? matcher)
    (and (= matcher (:key event))
         (empty? (:modifiers event)))
    (let [key-pressed (last matcher)
          mods (apply hash-set (drop-last matcher))]
      (and (= key-pressed (:key event))
           (= mods (:modifiers event))))))

(defmacro exec-press [& body]
  (let [body-full
        (if (even? (count body)) 
          (concat body ['c/stop-insert])
          body)]
    `(if-let [handler# 
              (try
                (condp match-key (:last-press ~'machine)
                  ~@body-full)
                (catch IllegalArgumentException e#
                  nil))]
       ;; handler!
       (handler# ~'machine ~'state)
       ;; not handled
       ~'machine)))

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
  (exec-press))
;
(defmethod pressed-in-mode :default
  [machine state]
  (exec-press
    :esc c/stop-insert
    ;; just return the machine
    machine))

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
  [machine event state]
  (case (:key event)
    :shift (add-modifier machine :shift)
    :alt (add-modifier machine :alt)
    :command (add-modifier machine :cmd)
    :control (add-modifier machine :ctrl)
    ;; default
    (let [modded-event (translate-event machine event)
          current-bindings (-> machine :current-bindings)
          the-key (:key modded-event)]
      (if-let [branch (get current-bindings the-key)]
        (follow-key-branch machine state branch)
        (pressed-in-mode (assoc machine :last-press modded-event)
                         state)))))

(defn- process-release
  "Process key release. Returns the new value of the input machine"
  [machine event]
  (case (:key event)
    :shift (remove-modifier machine :shift)
    :alt (remove-modifier machine :alt)
    :command (remove-modifier machine :cmd)
    :control (remove-modifier machine :ctrl)
    ;; default
    machine))


;;
;; Public interface
;;

(defn describe-input
  [machine-atom]
  ;; TODO
  (let [machine @machine-atom]
    (str (select-keys machine [:current-bindings :mode]))))

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
