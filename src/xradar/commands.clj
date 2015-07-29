(ns ^{:author "Daniel Leong"
      :doc "Commands that can be bound. All commands
           should be of the form [machine state] where
           machine is the input state machine map, and
           state is the radar's state atom. The return
           value MUST be the new machine state, if any."}
  xradar.commands
  (:require [xradar
             [aircraft-selection :refer [aircraft-to-bindings bindings-to-aircraft]]
             [flight-plan :refer [open-flight-plan]]]))

;;
;; Constants
;;

;; whether to use the native input instead of our
;;  custom-drawn one. For now, native is preferred
(def use-native-input true)

(def move-distance 10)
(def zoom-distance 50)

(defn switch-mode
  [machine state new-mode]
  (assoc machine
         :mode new-mode
         :current-bindings (get-in machine [:bindings new-mode] {})))

(defmacro to-mode
  [new-mode]
  `(switch-mode ~'machine ~'state ~new-mode))

(defn start-insert
  [machine state]
  (to-mode :insert))

(defn stop-insert
  [machine state]
  ;; TODO clean up state
  (if (= :normal (:mode machine))
    ;; if this is called when already in normal mode,
    ;;  we clear the selected aircraft
    (swap! state #(assoc % :selected nil)))
  (assoc (to-mode :normal)
         :insert-buffer []))

(defn handle-insert
  [machine state]
  ;; FIXME
  (let [last-press (-> machine :last-press)]
    (case (:raw-key last-press)
      \backspace 
      (assoc machine
             :insert-buffer
             (drop-last (:insert-buffer machine)))
      \newline
      (let [buffer (str (:insert-buffer machine))]
        ;; TODO do something with buffer
        (stop-insert machine state))
      ;; default; append
      (assoc machine
           :insert-buffer
           (concat (:insert-buffer machine)
                   [(:raw-key last-press)])))))

(defn echo
  [machine state & values]
  ;; TODO
  (println values)
  (assoc (stop-insert machine state)
         :last-echo values))

(defmacro doecho
  [& args]
  `(echo ~'machine ~'state ~@args))

(defn eval-command
  [machine state raw]
  (let [raw-symbol? (or (symbol? raw) (string? raw))
        command (if raw-symbol?
                  (ns-resolve 'xradar.commands (symbol raw)))]
    (cond 
      ;; valid command? execute
      command (command machine state)
      ;; form? insert machine/state and execute
      (and (not raw-symbol?) (seq raw))
      (if-let [list-cmd (ns-resolve 'xradar.commands (first raw))]
        (apply list-cmd machine state (rest raw))
        (doecho (str "No such command:" (first raw))))
      ;; no such thing :(
      :else (doecho (str "No such command:" raw)))))

;;
;; Aircraft selection
;;

(defn start-select-aircraft
  [machine state]
  (let [craft (:aircraft @state)
        aircraft-selections (aircraft-to-bindings craft 'select-aircraft)]
    (swap! state #(assoc %
                         :craft-bindings 
                         (bindings-to-aircraft aircraft-selections)))
    (assoc (to-mode :select-aircraft)
           :current-bindings aircraft-selections)))

(defn select-aircraft
  [machine state cid]
  (swap! state #(assoc % 
                       :selected cid 
                       :craft-bindings {}))
  (to-mode :normal))

;;
;; Window toggling
;;

(defn toggle-flight-plan
  [machine state]
  (if-let [cid (:selected @state)]
    (do
      (open-flight-plan state cid)
      (to-mode :normal))
    (doecho "You must select an aircraft to edit its flight plan")))

;;
;; View commands
;;

(defn move-view 
  [machine state direction]
  (if-let [dir 
           (case direction
             :left {:x (- move-distance)}
             :up {:y (- move-distance)}
             :right {:x move-distance}
             :down {:y move-distance}
             nil)]
    ;; move!
    (do
      (swap! state 
             (fn [state modifier]
               (assoc state 
                      :camera
                      (merge-with + (:camera state) modifier)))
             dir)
      ;; return the machine unchanged
      machine)
    ;; invalid
    (doecho "Invalid direction " direction)))

(defn zoom-view
  [machine state direction]
  (if-let [dir
           (case direction
             :in (- zoom-distance)
             :out zoom-distance)]
    (do
      (swap! state
             (fn [state modifier]
               (assoc state
                      :zoom
                      (+ (:zoom state) modifier)))
             dir)
      ;; return the machine unchanged
      machine)
    ;; invalid
    (doecho "Invalid zoom direction " direction)))
