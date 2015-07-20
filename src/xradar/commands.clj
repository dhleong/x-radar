(ns ^{:author "Daniel Leong"
      :doc "Commands that can be bound. All commands
           should be of the form [machine state] where
           machine is the input state machine map, and
           state is the radar's state atom. The return
           value MUST be the new machine state, if any."}
  xradar.commands)

(defn start-insert
  [machine state]
  (assoc machine
         :mode
         :insert))

(defn handle-insert
  [machine state]
  ;; FIXME
  (assoc machine
         :insert-buffer
         (-> machine :last-press :key)))

(defn stop-insert
  [machine state]
  ;; TODO clean up state
  (assoc machine
         :mode
         :normal))

(defn start-select-aircraft
  [machine state]
  (assoc machine
         :mode
         :select-aircraft))

