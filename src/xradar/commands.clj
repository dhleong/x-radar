(ns ^{:author "Daniel Leong"
      :doc "Commands that can be bound. All commands
           should be of the form [machine state] where
           machine is the input state machine map, and
           state is the radar's state atom. The return
           value MUST be the new machine state, if any."}
  xradar.commands)

(defn switch-mode
  [machine state new-mode]
  (assoc machine
         :mode new-mode
         :current-bindings (get-in machine [:bindings new-mode] {})))

(defn eval-command
  [machine state raw]
  (if-let [command (ns-resolve 'xradar.commands (symbol raw))]
    (command machine state)
    (echo machine state (str "No such command: " raw))))

(defn echo
  [machine state & values]
  ;; TODO
  (println values))

(defn start-insert
  [machine state]
  (switch-mode machine state :insert))

(defn stop-insert
  [machine state]
  ;; TODO clean up state
  (assoc (switch-mode machine state :normal)
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
           (conj (:insert-buffer machine)
                 (:raw-key last-press))))))

(defn start-select-aircraft
  [machine state]
  (switch-mode machine state :select-aircraft))

