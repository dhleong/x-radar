(ns ^{:author "Daniel Leong"
      :doc "Private chat"}
  xradar.chat
  (:require [clojure.string :refer [lower-case split]]
            [clojure.java.io :as io]
            [quil.core :as q]
            [xradar
             [output :refer [append-output]]
             [network :refer [connected? get-controllers 
                              my-callsign send! send-to!]]
             [radar-util :refer [request-attention!]]
             [util :refer [object-for with-alpha]]]))


(defn- prefix-incoming
  [state cid & parts]
  (let [obj (object-for state cid)]
    (str (or (:callsign obj) "???")
         ": "
         (apply str parts))))

(defn- prefix-outgoing
  [state & parts]
  (str (my-callsign (:network @state))
       ": "
       (apply str parts)))

(defn append-incoming
  [state cid message & parts]
  (apply append-output 
         state 
         (prefix-incoming state cid message) 
         parts)
  (request-attention!)
  (let [{:keys [with]} parts
        chat-target (:current-output @state)]
    (when (and
            (not= :global chat-target)
            (not= chat-target with))
      (swap! (:pending-messages @state) inc))))

(defn append-prefixed
  [state message & parts]
  (apply append-output 
         state 
         (prefix-outgoing state message) 
         parts))


(defn send-chat!
  "Send the provided message as chat to the
  correct channel, etc. based on the current
  settings.
  If an aircraft is selected and
  output is in :global mode, the message will
  be sent with the selected aircraft's callsign
  prepended.
  If no aircraft is selected in :global mode, the
  message will be sent as normal.
  If output is filtered, regardless of the aircraft
  selection the message will be sent as a private
  message to the CID the filter is set to."
  [state message]
  (let [network (:network @state)
        is-connected? (connected? network)
        selected (:selected @state)
        output-filter (:current-output @state)] 
    (cond
      ;; are we even connected?
      (not is-connected?)
      (append-output state "ERR: Not Connected"
                     :color :error
                     :flag :status)
      ;; output filtered?
      (not= :global output-filter)
      (let [craft (object-for state output-filter)]
        (append-prefixed state message
                         :color :outgoing
                         :with output-filter
                         :with-label (:callsign craft))
        (send-to! network output-filter message))
      ;; aircraft selected?
      (not (nil? selected))
      (let [craft (object-for state selected)
            formatted (str (:callsign craft) ", " message)]
        (append-prefixed state formatted
                         :color :outgoing)
        (send! network formatted))
      ;; default
      :else
      (do
        (append-prefixed state message
                         :color :outgoing)
        (send! network message)))))

(defn receive-from-private
  "Call when a private chat is received"
  [state cid message]
  (append-incoming state cid message
                   :with cid
                   :with-label (:callsign (object-for state cid))))

(defn receive-from
  "Call when a public chat is received"
  [state cid message]
  (append-incoming state cid message))
