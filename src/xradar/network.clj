(ns ^{:author "Daniel Leong"
      :doc "Protocol for network actions"}
  xradar.network
  (:require [xradar.voice :as v]))

(defn configure-xvoice!
  "Utility to call the appropriate methods
  on an XVoice instance, given the config
  object from (config-voice!)"
  [voice config]
  (let [connection-name (:name config)
        relevant-parts (select-keys config [:prim :rx-v :tx-v])
        want-connected? (->> relevant-parts
                             vals
                             (some true?))
        is-connected? (v/connected? voice connection-name)]
    (cond
      ;; already where we want to be
      (= want-connected? is-connected?)
      (v/config! voice connection-name relevant-parts)
      ;; not connected, but want to be
      (and want-connected? (not is-connected?))
      (v/connect! voice config)
      ;; connected and don't want to be
      (and (not want-connected?) is-connected?)
      (v/disconnect! voice connection-name))))

(defprotocol XRadarNetwork
  "Protocol for network actions"
  (config-voice!
    [this config]
    "Configure a voice connection. Expects a map:
    {:freq '121.800' :server 'voice.nyartcc.org'
    :channel 'zny_1c' :prim true :name 'LGA_GND'
    :rx-t true :tx-t true :rx-v true :tx-v true}
    The rx/tx values are receiving and transmitting,
    respectively; the -t and -v refer to text and
    voice, respectively. If :prim is true, then that
    is to be the primary frequency.")
  (connected?
    [this]
    "Returns true if we have an active connection 
    to the network.")
  (connect!
    [this params]
    "Request a connection to the network using
    the given parameters.")
  (disconnect!
    [this]
    "Sever any active connection with the network.")
  (get-controllers
    [this]
    "Return a list of online controllers. Each
    is a map that looks like:
    {:cid <id> :callsign \"LGA_TWR\"}")
  (get-servers
    [this]
    "Return a map of ServerName->Info, where
    Info is itself a map containing:
    {:ip 'IP address'
     :location 'New York, USA'}")
  (handoff-accept
    [this cid]
    "Accept handoff of the aircraft with the given CID")
  (handoff-reject
    [this cid]
    "Reject handoff of the aircraft with the given CID")
  (handoff!
    [this cid]
    "Propose a handoff of the aircraft with the given CID")
  (my-callsign
    [this]
    "Return the callsign we connected as")
  (push-strip!
    [this controller-id aircraft]
    "Push a flight strip to the controller with
    the given id")
  (request-atis
    [this controller-id]
    "Request ATIS from the controller with the given id")
  (request-metar
    [this airport-icao]
    "Request METAR updates for the given airport")
  (send!
    [this message]
    "Send a message on the active frequency")
  (send-to!
    [this cid message]
    "Send a private message to the provided pilot")
  (stop-request-metar
    [this airport-icao]
    "STOP requesting METAR updates for the given airport")
  (update-flightplan
    [this aircraft]
    "Update the provided aircraft's flightplan"))
