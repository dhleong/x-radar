(ns ^{:author "Daniel Leong"
      :doc "Commands that can be bound. All commands
           should be of the form [machine state] where
           machine is the input state machine map, and
           state is the radar's state atom. The return
           value MUST be the new machine state, if any."}
  xradar.commands
  (:require [clojure
             [edn :as edn]
             [string :refer [upper-case]]
             [test :refer [function?]]]
            [quil.core :as q]
            [seesaw.core :as s]
            [xradar
             [aircraft :refer [describe-craft]]
             [chat :refer [send-chat!]]
             [connection-config :refer [open-connection]]
             [flight-plan :refer [open-flight-plan]]
             [flight-strips :as fs]
             [lists :refer [toggle-list]]
             [native-insert :refer [create-insert input-height]]
             [network :refer [connect! connected? disconnect!
                              get-controllers push-strip!
                              request-atis
                              update-flightplan]]
             [notif :refer [ack-attention!]]
             [output :refer [append-output 
                             get-active get-active-buffer 
                             scroll-output! set-active!]]
             [profile :refer [commit-profile]]
             [radar-util :refer [get-location redraw update-aircraft]]
             [scene :refer [find-point]]
             [selection :refer [to-bindings from-bindings]]
             [selection-mode :as sm]
             [util :refer [deep-merge in-bounds]]
             [voice :as v]
             [voice-config :refer [open-voice-comms]]
             [weather :refer [mark-acked! unwatch-weather!
                              watch-weather!]]]
            [xradar.modes.mode-util :as mu])
  (:import [java.io StringReader PushbackReader]))

;;
;; Constants
;;

(def move-distance 10)
(def zoom-distance 50)

;; candidates for prompt-reply-private-chat will be taken
;;  from this many recent output lines
(def prompt-reply-limit 15)

(defn- default-input-submit
  [machine state message]
  (swap! (:history-insert @state) 
         (fn [history]
           (cons message history)))
  (send-chat! state message))


;;
;; Util methods and macros
;;

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
          :last-echo (str ~@values)))

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
            (try
              (apply list-cmd machine state (rest raw))
              (catch Exception e
                (.printStackTrace e)
                (append-output state 
                               (str "ERR: " list-cmd " failed: " (.getMessage e))
                               :color :error)
                (notify-mode :normal "Unable to execute " list-cmd)))
            (notify-mode :normal "No such command:" (first raw)))
          ;; no such thing :(
          :else (notify-mode :normal "No such command:" raw))]
    (redraw state)
    ;; always clear the current sequence
    ;;  when we evaluate a command
    (assoc updated-machine
           :current-sequence [])))

(defn- resolve-keyword
  [raw-keyword]
  (cond
    (keyword? raw-keyword)
    raw-keyword
    (symbol? raw-keyword)
    (keyword raw-keyword)
    (= \: (first raw-keyword))
    (keyword (subs raw-keyword 1))
    :else (keyword raw-keyword)))

;;
;; Insert mode handling
;;

(defn start-insert
  "Start insert mode to get some text input from the user.
  Optionally you can provide a textual `prompt`, and a 
  handler to be fired `on-submit`. 
  `initial` is the initial content of the box
  `initial-selected` If true, select the initial contents of the box
  `on-submit` should be a (fn [machine state value]) 
  where `machine` and `state` are the usual command args,
  and `value` is the string value provided by the user.
  If an updated machine is not provided, we will perform
  a switch into :normal mode
  `on-change` if provided should be a (fn [state value])
  where `state` is the usual command arg (Note that you 
  cannot alter the machine from `on-change`!) and `value`
  is the current input from the user.
  `cancel-mode` should be the keyword for a mode to return
  to on cancel. If not provided, defaults to `:normal`"
  [machine state & {:keys [history prompt 
                           initial initial-selected
                           on-submit on-change
                           cancel-mode]
                    :or {initial ""
                         initial-selected false}}]
  (let [{:keys [x y]} (get-location state)
        selected-id (:selected @state)
        selected (get (:aircraft @state) selected-id nil)
        active-chat (get-active state)
        ;; NB don't change modes if we have a custom on-submit.
        ;; Especially since we're using native input...
        moded (if on-submit
                machine
                (to-mode :insert))
        my-prompt 
        (cond
          (string? prompt) prompt
          (and
            (= :global active-chat)
            (not (nil? selected))) (str ">" (:callsign selected))
          (not= :global active-chat) (str ">" (:callsign active-chat))
          :else nil)
        submit-handler (or on-submit default-input-submit)
        change-handler (or on-change (constantly nil))]
    (assoc moded
           :insert-box 
           (create-insert 
             state
             x 
             (+ y (q/height) (- input-height)) 
             (q/width)
             :prompt my-prompt
             :history (or history @(:history-insert @state))
             :initial initial
             :initial-selected initial-selected
             :on-cancel #(with-machine (to-mode (or cancel-mode :normal)))
             :on-change #(try
                           (change-handler state %)
                           (catch Exception e
                             (def last-change-exc e)))
             :on-submit 
             #(let [result 
                    (try
                      (submit-handler machine state %)
                      (catch Exception e
                        (def last-exc e)
                        (notify-mode :normal "Error" (.getMessage e))))
                    new-machine
                    (if (:mode result)
                      result
                      (-> (to-mode :normal)))]
                ;; not sure why this was deep-merge...
                ;;  it breaks key stroke handling. Repro
                ;;  is `wa` to ack a weather change; it will
                ;;  then have the :call for `wa` in the root `w`,
                ;;  breaking things until you hit `esc`
                (with-machine (merge machine new-machine))
                (redraw state))))))

(defn stop-insert
  [machine state]
  (if (= :normal (:mode machine))
    ;; if this is called when already in normal mode,
    ;;  we clear the selected aircraft
    (swap! state #(assoc % :selected nil)))
  (assoc (to-mode :normal)
         :insert-buffer []))

;;
;; Command Mode
;;

(defn start-command
  ([machine state]
   (start-insert machine state
                 :prompt " :"
                 :history @(:history-command @state)
                 :on-submit start-command))
  ([machine state raw-command]
   (when-let [trimmed (.trim raw-command)]
     (let [input 
           (if (and (.contains trimmed " ")
                    (not= \( (first trimmed)))
             (str "(" trimmed ")")
             trimmed)
           parsed (edn/read-string input)]
       (try
         (swap! (:history-command @state) 
                (fn [history]
                  (cons input history)))
         (eval-command machine state parsed)
         (catch Exception e
           (doecho "Error: " (.getMessage e))))))))

;;
;; Selection mode
;;

(defn start-select
  [machine state & args]
  (apply sm/start machine state args))

;;
;; Search mode
;;

(declare cancel-toggle-chat)
(declare select-aircraft)

(defn- search-filtered-aircraft
  [state search-filter]
  (filter 
    #(.contains (:callsign %) search-filter)
    (vals (:aircraft @state))))

(defn- select-aircraft-filter
  "Start or update select mode with
  aircraft, based on a search param"
  [state search-filter & [machine]]
  (let [targets (search-filtered-aircraft state search-filter)]
    (redraw state)
    (start-select (or machine {}) state
                  :items targets
                  :to-string #(:callsign %)
                  :on-cancel 'cancel-toggle-chat
                  :on-select 'select-aircraft)))


(defn start-search-aircraft
  ([machine state]
   (let [targets (vals (:aircraft @state))
         insert-mac
         (start-insert machine state
                       :prompt " /"
                       :history []
                       :on-change select-aircraft-filter
                       :on-submit start-search-aircraft)
         select-mac
         (select-aircraft-filter state "")]
     (merge insert-mac select-mac)))
  ([machine state search-param]
   (let [targets (search-filtered-aircraft state search-param)]
     (case (count targets)
       0 (notify-mode :normal "No matching aircraft")
       1 (select-aircraft machine state (first targets))
       (select-aircraft-filter state search-param machine)))))

;;
;; Output navigation
;;

(defn output-scroll
  [machine state amount]
  (scroll-output! state amount)
  ;; no change in machine besides resetting sequence
  (to-mode :normal))

;;
;; Aircraft config/editing
;;

(defn config
  ([machine state]
   (notify-mode :normal "Incomplete"))
  ([machine state raw-field]
   (let [cid (:selected @state)
         craft (get-in @state [:aircraft cid])
         callsign (:callsign craft)
         field (resolve-keyword raw-field)]
     (if callsign
       (start-insert machine state
                     :prompt (str "Update " callsign " " field)
                     :history []
                     :initial (str (get craft field ""))
                     :initial-selected true
                     :on-submit #(config %1 %2 raw-field %3))
       (notify-mode :normal "You must select an aircraft first"))))
  ([machine state raw-field value]
   (let [cid (:selected @state)
         craft (get-in @state [:aircraft cid])
         callsign (:callsign craft)
         field (resolve-keyword raw-field)
         network (:network @state)]
     (if cid
       (let [updated-values {:callsign callsign
                             :cid cid
                             field (str value)}]
         (swap! state 
                deep-merge 
                {:aircraft {cid updated-values}})
         (update-flightplan network updated-values)
         (notify-mode :normal "Updated " raw-field))
       (notify-mode :normal "You must select an aircraft first")))))

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
  [machine state cid-or-object]
  (let [cid (if (map? cid-or-object)
              (:cid cid-or-object)
              cid-or-object)]
    (swap! state #(assoc % 
                        :selected cid 
                        :craft-bindings {})))
  (let [obj (if (map? cid-or-object)
              cid-or-object
              (get-in @state [:aircraft cid-or-object]))]
    (notify-mode :normal (describe-craft @state obj))))

;;
;; ATIS-related
;;

(defn atis
  "Request a Controller's ATIS"
  ([machine state]
   (let [network (:network @state)
         targets (get-controllers network)]
    (if-not (empty? targets)
      (start-select machine state
                    :items targets
                    :prompt "Request ATIS from:"
                    :to-string #(:callsign %)
                    :on-cancel 'cancel-atis-request
                    :on-select 'atis)
      (doecho "No other controllers to query"))))
  ([machine state callsign-or-obj]
   (let [callsign (if (string? callsign-or-obj)
                    callsign-or-obj
                    (:callsign callsign-or-obj))]
     (request-atis (:network @state) callsign)
     (notify-mode :normal "Requested ATIS from " callsign))))

(defn cancel-atis-request
  [machine state]
  (to-mode :normal))

;;
;; Chat commands
;;

(defn cancel-toggle-chat
  [machine state]
  (to-mode :normal))

(defmulti toggle-private-chat
  "Open a private chat. Pass :pilot to open
  selections for a pilot, :controller to open for
  a controller, or a cid to open directly with that id."
  (fn [machine state arg]
    arg))
(defmethod toggle-private-chat :pilot 
  [machine state _]
  (let [targets (vals (:aircraft @state))]
    (start-select machine state
                  :items targets
                  :prompt "Chat with pilot:"
                  :to-string #(:callsign %)
                  :on-cancel 'cancel-toggle-chat
                  :on-select 'toggle-private-chat)))
(defmethod toggle-private-chat :controller
  [machine state _]
  (let [targets (get-controllers (:network @state))]
    (start-select machine state
                  :items targets
                  :prompt "Chat with controller:"
                  :to-string #(:callsign %)
                  :on-cancel 'cancel-toggle-chat
                  :on-select 'toggle-private-chat)))
(defmethod toggle-private-chat :default
  [machine state cid-or-object]
  (let [cid (if (map? cid-or-object)
              (:cid cid-or-object)
              cid-or-object)]
    (def setting-active cid)
    (set-active! state cid)
    (to-mode :normal)))

(defn toggle-combined-chat
  "Leave a private chat and show all chats combined"
  [machine state]
  (set-active! state :global)
  (to-mode :normal))

(defn toggle-selected-chat
  "Switch between a private chat with the selected
  aircraft and the global chat."
  [machine state]
  (let [chat-target (get-active state)
        selected (:selected @state)]
    (def mytarget chat-target)
    (def mysel selected)
    (cond
      (nil? selected)
      (notify-mode :normal "You must select an aircraft")
      (= :global chat-target)
      (toggle-private-chat machine state selected)
      :else (toggle-combined-chat machine state))))

(defn reply-private-chat
  "Reply to the most recently received private chat message
  within the current context. That is, when already filtered
  to a private chat, this will do the same as start-insert.
  You will be left in filtered private chat mode."
  [machine state]
  (when (= :global (get-active state))
    (when-let [first-private (->> (get-active-buffer @state)
                             (filter #(not (nil? (:with %))))
                             first)]
      (set-active! state (:with first-private))))
  (start-insert machine state))

(defn prompt-reply-private-chat
  "Reply to a received private chat message. You will 
  be prompted to choose the recipient even if already
  filtered to a private chat. This command is different 
  from toggle-private-chat in that the candidates are
  only those with whom you've received or to whom you've
  sent a private chat message recently."
  ([machine state]
   (let [targets (->> @(:output-buffer @state)
                      (take prompt-reply-limit)
                      (filter #(not (nil? (:with %))))
                      (map #(select-keys % [:with :with-label]))
                      distinct)]
     (if (seq targets)
       (start-select machine state
                     :items targets
                     :prompt "Reply to:"
                     :to-string #(:with-label %)
                     :on-cancel 'cancel-toggle-chat
                     :on-select 'prompt-reply-private-chat)
       (doecho "Nobody to reply to recently"))))
  ([machine state cid-or-line]
   (let [cid (if (map? cid-or-line)
               (:with cid-or-line)
               cid-or-line)]
     (set-active! state cid)
     (start-insert machine state))))

;;
;; Data management
;;

(defn commit
  "Write updated settings file to disk."
  [machine state]
  (commit-profile state)
  (doecho "Settings written to disk."))

;;
;; Info box manipulation
;;

(defn info-box-length
  ([machine state]
   (info-box-length machine state nil))
  ([machine state adjustment]
   (if-let [cid (:selected @state)]
    (do
      (mu/info-line-length! state cid adjustment)
      (to-mode :normal))
    (notify-mode :normal "You must select an aircraft to adjust its info box"))))

(defn info-box-rotate
  ([machine state]
   (info-box-rotate machine state nil))
  ([machine state adjustment]
   (if-let [cid (:selected @state)]
    (do
      (mu/info-line-rotate! state cid adjustment)
      (to-mode :normal))
    (notify-mode :normal "You must select an aircraft to adjust its info box"))))


;;
;; Window toggling
;;

(defn toggle-arrivals
  "Show/hide the list of arrivals"
  [machine state]
  (toggle-list state :arrivals)
  (to-mode :normal))

(defn toggle-departures
  "Show/hide the list of departures"
  [machine state]
  (toggle-list state :departures)
  (to-mode :normal))


(defn toggle-flight-plan
  [machine state]
  (if-let [cid (:selected @state)]
    (do
      (open-flight-plan state cid)
      (to-mode :normal))
    (notify-mode :normal "You must select an aircraft to edit its flight plan")))

(defn toggle-voice-config
  [machine state]
  (open-voice-comms state)
  ;; just reset the mode
  (to-mode :normal))

(defn connect
  ([machine state]
   (let [network (:network @state)]
     (if (connected? network)
       ;; confirm disconnect
       (when (= :success
                (-> (s/dialog :content "Already connected; disconnect?" 
                              :option-type :yes-no)
                    s/pack!
                    s/show!))
         (disconnect! network)
         (append-output state "Disconnected." 
                        :color :warning
                        :flag :status)
         machine)
       ;; not connected! go ahead
       (open-connection
         state
         #(with-machine (connect machine state %)))))
   ;; just reset the mode
   (to-mode :normal))
  ([machine state params]
   (append-output state "Connecting..."
                  :flag :status)
   (connect! (:network @state) 
             (assoc params
                    :on-fail #(append-output state "Failed to connect."
                                             :color :warning
                                             :flag :status)
                    :on-connect #(append-output state "Connected!"
                                                :color :success
                                                :flag :status)))
   machine))

;;
;; View commands
;;

(defn center-view
  ([machine state]
   (start-insert machine state 
                 :prompt "Center On:"
                 :history []
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
                 :history []
                 :cancel-mode :strips
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
          :strips)
        in-strips? (= :strips new-mode)] 
    (when in-strips?
      (ack-attention! :fp))
    (if (and in-strips?
             (fs/bays-empty? (:strips @state)))
      (notify-mode :normal "No flight strips")
      (to-mode new-mode))))

;;
;; Voice transmission
;;

(defn transmit-voice
  [machine state]
  (let [voice (:voice @state)]
    (def bla? (v/connected? voice))
    (cond
      (not (v/connected? voice))
      (doecho "No primary voice channel connected")
      (v/transmitting? voice) 
      machine ;; already transmitting; do nothing
      :else (do
              (v/start-transmitting voice)
              (redraw state)
              machine))))

(defn stop-transmit-voice
  [machine state]
  (let [voice (:voice @state)]
    (def stop? true)
    (cond
      (not (v/connected? voice))
      (doecho "No primary voice channel connected")
      (not (v/transmitting? voice))
      machine ;; not transmitting; do nothing
      :else (do
              (v/stop-transmitting voice)
              (redraw state)
              machine))))

;;
;; Weather commands
;;

(defn weather-ack
  "Acknowledge the changed weather at a given airport"
  ([machine state]
   (start-insert machine state 
                 :prompt "Acknowledge Airport Weather:"
                 :history []
                 :on-submit weather-ack))
  ([machine state icao]
   (mark-acked! icao)
   (redraw state)
   (notify-mode :normal "Acknowledged weather for " (upper-case icao))))

(defn weather-delete
  "Remove weather watch at a given airport"
  ([machine state]
   (start-insert machine state 
                 :prompt "Remove Airport Weather:"
                 :history []
                 :on-submit weather-delete))
  ([machine state icao]
   (unwatch-weather! icao)
   (redraw state)
   (notify-mode :normal "Removed watch for " (upper-case icao))))

(defn weather-watch
  "Watch the weather at a given airport"
  ([machine state]
   (start-insert machine state 
                 :prompt "Watch Airport Weather:"
                 :history []
                 :on-submit weather-watch))
  ([machine state icao]
   (watch-weather! icao)
   (redraw state)
   (notify-mode :normal "Watching weather at " (upper-case icao))))

(defn weather-toggle-metar
  "Toggle showing the full metar for an airport"
  ([machine state]
   (start-insert machine state 
                 :prompt "Toggle airport METAR:"
                 :history []
                 :on-submit weather-toggle-metar))
  ([machine state raw-icao]
   (let [icao (upper-case raw-icao)]
     (swap! state #(assoc
                     %
                     :shown-metar 
                     (if (or (empty? raw-icao)
                             (= icao (:shown-metar %)))
                       nil ;; no more metar
                       icao)))
   (redraw state)
   (to-mode :normal))))
