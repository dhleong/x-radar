(ns ^{:author "Daniel Leong"
      :doc "Commands that can be bound. All commands
           should be of the form [machine state] where
           machine is the input state machine map, and
           state is the radar's state atom. The return
           value MUST be the new machine state, if any."}
  xradar.commands
  (:require [quil.core :as q]
            [xradar
             [aircraft-selection :refer [aircraft-to-bindings bindings-to-aircraft]]
             [flight-plan :refer [open-flight-plan]]
             [native-insert :refer [create-insert input-height]]
             [network :refer [send! send-to!]]
             [output :refer [append-output]]
             [radar-util :refer [get-location redraw]]
             [scene :refer [find-point]]
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
         :current-bindings (get-in machine [:bindings new-mode] {})))

(defmacro to-mode
  [new-mode]
  `(switch-mode ~'machine ~'state ~new-mode))

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
          my-prompt 
          (cond
            (string? prompt) prompt
            (not (nil? selected)) (str ">" (:callsign selected))
            :else nil)
          submit-handler (or on-submit default-input-submit)]
      (assoc (to-mode :insert)
             :insert-box 
             (create-insert 
               x 
               (+ y (q/height) (- input-height)) 
               (q/width)
               :prompt my-prompt
               :on-cancel #(with-machine (to-mode :normal))
               :on-submit 
               #(with-machine
                  (submit-handler state %)
                  (-> (to-mode :normal))))))
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
;; Aircraft selection
;;

(defn start-select-aircraft
  [machine state]
  (let [craft 
        (->> (:aircraft @state)
             vals
             #_(map #(in-bounds (:x %) (:y %)))
             (map :cid))
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

(defn center-view
  ([machine state]
   (start-insert machine state 
                 :prompt "Center On:"
                 :on-submit #(with-machine
                               (center-view machine %1 %2))))
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
