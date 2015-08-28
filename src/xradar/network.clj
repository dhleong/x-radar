(ns ^{:author "Daniel Leong"
      :doc "Protocol for network actions"}
  xradar.network)

(defprotocol XRadarNetwork
  "Protocol for network actions"
  (config-voice!
    [this config]
    "Configure a voice connection. Expects a map:
    {:freq '121.800' :server 'voice.nyartcc.org'
    :channel 'zny_1c' :prim true 
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
  (my-callsign
    [this]
    "Return the callsign we connected as")
  (push-strip!
    [this controller-id aircraft]
    "Push a flight strip to the controller with
    the given id")
  (send!
    [this message]
    "Send a message on the active frequency")
  (send-to!
    [this cid message]
    "Send a private message to the provided pilot")
  (update-flightplan
    [this aircraft]
    "Update the provided aircraft's flightplan"))
