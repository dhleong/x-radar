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
             [util :refer [with-alpha]]]))

(defn- cid-to-controller
  [state cid]
  (let [network (:network @state)
        controllers (get-controllers network)]
    (->> controllers
        (filter #(= cid (:cid %)))
        first)))

(defn object-for
  "Returns the pilot/controller object of the 
  given cid"
  [state cid]
  (get-in @state 
          [:aircraft cid]
          ;; try controller
          (cid-to-controller state cid)))


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
         parts))

(defn append-prefixed
  [state message & parts]
  (apply append-output 
         state 
         (prefix-outgoing state message) 
         parts))


(defn selected-chat
  "Returns the pilot/controller object of the 
  currently-selected chat, or :global if viewing 
  global chat."
  [state]
  (let [current (:current-output @state)]
    (if (= :global current)
      :global
      (object-for state current))))

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
