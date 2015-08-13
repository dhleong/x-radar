(ns ^{:author "Daniel Leong"
      :doc "Protocol for network actions"}
  xradar.network)

(defprotocol XRadarNetwork
  "Protocol for network actions"
  (connect!
    [this & {:keys [on-connect on-fail
                    callsign real-name
                    facility rating
                    cid pass
                    server]}]
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
  (is-connected?
    [this]
    "Returns true if we have an active connection 
    to the network.")
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
