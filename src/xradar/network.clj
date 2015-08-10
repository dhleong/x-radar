(ns ^{:author "Daniel Leong"
      :doc "Protocol for network actions"}
  xradar.network)

(defprotocol XRadarNetwork
  "Protocol for network actions"
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
