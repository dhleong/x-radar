(ns ^{:author "Daniel Leong"
      :doc "Handoff support"}
  xradar.handoff
  (:require [clojure.string :refer [lower-case split]]
            [quil
             [core :as q]]
            [xradar
             [network :as n]
             [notif :refer [ack-attention! request-attention!]]
             [radar-util :refer [redraw]]
             [scene :refer :all]
             [util :refer [coord-scale deep-merge in-bounds map-coord]] ]))

(defonce pending-handoffs (atom #{}))

(defn- clear-pending
  "Clear a pending handoff request. Returns true
  if it worked, else false"
  [cid]
  (if (contains? @pending-handoffs cid)
    (let [new-pending (swap! pending-handoffs disj cid)]
      (when (empty? new-pending) 
        (ack-attention! :handoff))
      true)
    false))

(defn on-handoff
  "Call when a new handoff has arrived"
  [state cid]
  (let [new-pending (swap! pending-handoffs conj cid)]
    (when (= 1 (count new-pending)) 
      (request-attention! :topic :handoff :is-critical true))
    (when state ;; basically for testing
      (redraw state))))

(defn accept-handoff
  "Accept handoff of the given aircraft.
  Returns true when there was such a handoff
  to accept, else nil"
  [state cid]
  (when (clear-pending cid)
    (n/handoff-accept (:network @state) cid)
    true))

(defn propose-handoff
  [state cid receiver]
  (n/handoff! (:network @state) cid))

(defn reject-handoff
  "Reject handoff of the given aircraft
  Returns true when there was such a handoff
  to reject, else nil"
  [state cid]
  (when (clear-pending cid)
    (n/handoff-reject (:network @state) cid)
    true))
