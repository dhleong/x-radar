(ns ^{:author "Daniel Leong"
      :doc "Commands that can be bound. All commands
           should be of the form [machine state] where
           machine is the input state machine map, and
           state is the radar's state atom. The return
           value MUST be the new machine state, if any."}
  xradar.commands
  (:require [clojure.test :refer [function?]]
            [quil.core :as q]
            [xradar
             [flight-plan :refer [open-flight-plan]]
             [flight-strips :as fs]
             [native-insert :refer [create-insert input-height]]
             [network :refer [get-controllers push-strip! send! send-to!]]
             [output :refer [append-output]]
             [radar-util :refer [get-location redraw]]
             [scene :refer [find-point]]
             [selection :refer [to-bindings from-bindings]]
             [selection-mode :as sm]
             [util :refer [in-bounds]]]))

;;
;; Constants
;;

;; whether to use the native input instead of our
;;  custom-drawn one. For now, native is preferred
(def use-native-input true)

(def move-distance 10)
(def zoom-distance 50)

(defn- default-input-submit
  [state message]
  (try
    (let [network (:network @state)] 
      (if-let [selected (:selected @state)]
        (let [craft (get (:aircraft @state) selected {:callsign selected})]
          (append-output state (str (:callsign craft) ", " message)
                         :color :outgoing)
          (send-to! network selected message))
        (do
          (append-output state message
                         :color :outgoing)
          (send! network message))))
    (catch Throwable e
      (def last-exc e))))

;;
;; Util methods and macros
;;

(defn set-use-native-input! [do-use]
  (def use-native-input do-use))

(defn switch-mode
  [machine state new-mode]
  (assoc machine
         :mode new-mode
         :current-bindings (get-in machine [:bindings new-mode] {})
         :current-sequence []))

(defmacro to-mode
  [new-mode]
  `(switch-mode ~'machine ~'state ~new-mode))

(defmacro notify-mode
  "The preferred way to swap modes AND
  echo a message explaining why."
  [new-mode & values]
  `(assoc (to-mode ~new-mode)
          :last-echo ~@values))

(defmacro with-machine
  "When all you have is the radar state and want to
  update the machine state, you can use this macro.
  The body forms provided will be wrapped in a function
  that is called like a normal input function with the
  machine map and the state atom; the result of this
  function call will be the new machine value, and
  a redraw will be requested."
  [& body]
  `(swap! (:input @~'state) 
          (fn [~'machine ~'state]
            (redraw ~'state)
            ~@body)
          ~'state))

(defn echo
  [machine state & values]
  ;; (assoc (stop-insert machine state)
  ;;        :last-echo values)
  (redraw state)
  (assoc machine :last-echo values))

(defmacro doecho
  [& args]
  `(echo ~'machine ~'state ~@args))

(defn eval-command
  [machine state raw]
  (let [raw-symbol? (or (symbol? raw) (string? raw))
        command (if raw-symbol?
                  (ns-resolve 'xradar.commands (symbol raw)))
        updated-machine
        (cond 
          ;; valid command? execute
          command (command machine state)
          ;; function? call it
          (function? command) (command)
          ;; form? insert machine/state and execute
          (and (not raw-symbol?) (seq raw))
          (if-let [list-cmd (ns-resolve 'xradar.commands (first raw))]
            (apply list-cmd machine state (rest raw))
            (notify-mode :normal
                         (str "No such command:" (first raw))))
          ;; no such thing :(
          :else (notify-mode :normal
                             (str "No such command:" raw)))]
    ;; always clear the current sequence
    ;;  when we evaluate a command
    (assoc updated-machine
           :current-sequence [])))

;;
;; Insert mode handling
;;

(defn start-insert
  "Start insert mode to get some text input from the user.
  Optionally you can provide a textual `prompt`, and a 
  handler to be fired `on-submit`. 
  `on-submit` should be a (fn [state value]) where `state` 
  is the usual radar state atom, and `value` is the string
  value provided by the user."
  [machine state & {:keys [prompt on-submit]}]
  (if use-native-input
    ;; build our input dialog
    (let [{:keys [x y]} (get-location state)
          selected-id (:selected @state)
          selected (get (:aircraft @state) selected-id nil)
          ;; NB don't change modes if we have a custom on-submit.
          ;; Especially since we're using native input...
          moded (if on-submit
                  machine
                  (to-mode :insert))
          my-prompt 
          (cond
            (string? prompt) prompt
            (not (nil? selected)) (str ">" (:callsign selected))
            :else nil)
          submit-handler (or on-submit default-input-submit)]
      (assoc moded
             :insert-box 
             (create-insert 
               x 
               (+ y (q/height) (- input-height)) 
               (q/width)
               :prompt my-prompt
               :on-cancel #(with-machine (to-mode :normal))
               :on-submit 
               #(with-machine
                  (let [new-machine (submit-handler machine state %)]
                    (redraw state)
                    (if (:mode new-machine)
                     new-machine
                     (-> (to-mode :normal))))))))
    ;; just the unfinished, custom input handling
    (to-mode :insert)))

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

;;
;; Output navigation
;;

(defn output-scroll
  [machine state amount]
  (swap! state 
         #(let [outputs (count @(:output-buffer @state))
                output-size (-> @state :profile :output-size)
                last-scroll (:output-scroll %)
                new-scroll (+ amount last-scroll)
                adjusted (-> new-scroll
                             (max 0)
                             (min (- outputs output-size)))]
            (assoc % :output-scroll adjusted)))
  ;; no change in machine
  machine)

;;
;; Aircraft selection
;;

(defn start-select-aircraft
  [machine state]
  (let [craft 
        (->> (:aircraft @state)
             vals
             ;; NB: this doesn't work, since the
             ;;  camera will have been reset
             ;;  at the point this is called
             #_(map #(in-bounds (:x %) (:y %)))
             (map :cid))
        aircraft-selections (to-bindings craft 'select-aircraft)]
    (swap! state #(assoc %
                         :craft-bindings 
                         (from-bindings aircraft-selections)))
    (assoc (to-mode :select-aircraft)
           :current-bindings aircraft-selections)))

(defn select-aircraft
  [machine state cid]
  (swap! state #(assoc % 
                       :selected cid 
                       :craft-bindings {}))
  (to-mode :normal))

;;
;; Selection mode
;;

(defn start-select
  [machine state & args]
  (apply sm/start machine state args))

;;
;; Window toggling
;;

(defn toggle-flight-plan
  [machine state]
  (if-let [cid (:selected @state)]
    (do
      (open-flight-plan state cid)
      (to-mode :normal))
    (notify-mode :normal "You must select an aircraft to edit its flight plan")))

;;
;; View commands
;;

(defn center-view
  ([machine state]
   (start-insert machine state 
                 :prompt "Center On:"
                 :on-submit center-view))
  ([machine state point-name]
   (if-let [point
            (find-point (:scene @state) point-name)]
     (do 
       (swap! state 
              #(assoc %1 :camera %2)
              (:coord point))
       ;; return the machine unchanged
       machine)
     ;; FIXME we need a better system for callbacks.
     ;;  Having to do this to avoid deadlock is obnoxious
     (do (future (with-machine
                   (doecho "Invalid location " point-name)))
         machine))))

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

;;
;; Flight strip commands
;;

(defn add-strip
  [machine state]
  (if-let [cid (:selected @state)]
    (do 
      (fs/add-strip (:strips @state) cid)
      (doecho "Added flight strip"))
    (doecho "You must select an aircraft to add its flight strip")))

(defn add-strip-separator
  ([machine state]
   (start-insert machine state 
                 :prompt "Separator label:"
                 :on-submit add-strip-separator))
  ([machine state label]
   (fs/add-separator (:strips @state) label)
   ;; make sure we stay in strips mode
   (to-mode :strips)))

(defn delete-current-strip
  [machine state]
  (let [strips (:strips @state)]
    (fs/delete-current-strip strips)
    (if (fs/bays-empty? strips)
      (to-mode :normal)
      machine)))

(defn edit-current-strip
  [machine state]
  (let [bay-atom (:strips @state)]
    (if-let [cid (fs/get-current-strip bay-atom)]
      (do
        (open-flight-plan state cid)
        ;; NB stay in flight strip mode
        machine)
      (doecho "No selected strip"))))

(defn cancel-push-current-strip
  [machine state]
  (to-mode :strips))

(defn push-current-strip-to-controller
  "Callback command for push-current-strip"
  [machine state controller]
  (let [bay-atom (:strips @state)
        network (:network @state)]
    (when-let [cid (fs/get-current-strip bay-atom)]
      (when-let [craft (get (:aircraft @state) cid)]
        (push-strip! network (:cid controller) craft)
        (redraw state)
        (notify-mode 
          :strips
          (str "Pushed " (:callsign craft) 
               " to " (:callsign controller)))))))

(defn push-current-strip
  [machine state]
  (let [bay-atom (:strips @state)
        network (:network @state)
        targets (get-controllers network)]
    (if-not (empty? targets)
      (if-let [cid (fs/get-current-strip bay-atom)]
        (start-select machine state
          :items targets
          :prompt "Receiving controller:"
          :to-string #(:callsign %)
          :on-cancel 'cancel-push-current-strip
          :on-select 'push-current-strip-to-controller)
        (doecho "No selected strip"))
      (doecho "No other controllers to push to"))))

(defn move-strip-cursor
  [machine state direction]
  (fs/move-strip-cursor (:strips @state) direction)
  machine)

(defn move-current-strip
  [machine state direction]
  (fs/move-current-strip (:strips @state) direction)
  machine)

(defn toggle-flight-strips
  [machine state]
  (let [new-mode 
        (case (:mode machine)
          :strips :normal
          :strips)] 
    (if (and (= :strips new-mode)
             (fs/bays-empty? (:strips @state)))
      (notify-mode :normal "No flight strips")
      (to-mode new-mode))))
