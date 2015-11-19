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

(defonce pending-handoffs (atom {}))

(defn clear-pending
  "Clear a pending handoff request. Returns the
  proposer's cid (the :from of the handoff) on success.
  Mostly public for testing; clients should use
  (reject-handoff)"
  [cid]
  (when-let [handoff (get @pending-handoffs cid)]
    (let [new-pending (swap! pending-handoffs dissoc cid)]
      (when (empty? new-pending) 
        (ack-attention! :handoff))
      (:from handoff))))

(defn on-handoff
  "Call when a new handoff has arrived"
  [state handoff]
  (let [new-pending (swap! pending-handoffs 
                           assoc 
                           (:cid handoff) handoff)]
    (when (= 1 (count new-pending)) 
      (request-attention! :topic :handoff :is-critical true))
    (when state ;; basically for testing
      (redraw state))))

(defn accept-handoff
  "Accept handoff of the given aircraft.
  Returns true when there was such a handoff
  to accept, else nil"
  [state cid]
  (when-let [proposer (clear-pending cid)]
    (n/handoff-accept (:network @state) proposer cid)
    true))

(defn propose-handoff
  [state cid receiver]
  (n/handoff! (:network @state) receiver cid))

(defn proposed?
  "Check if another controller has proposed a
  handoff of the cid"
  [cid]
  (contains? @pending-handoffs cid))

(defn reject-handoff
  "Reject handoff of the given aircraft
  Returns true when there was such a handoff
  to reject, else nil"
  [state cid]
  (when-let [proposer (clear-pending cid)]
    (n/handoff-reject (:network @state) proposer cid)
    true))
