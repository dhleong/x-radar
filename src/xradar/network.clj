(ns ^{:author "Daniel Leong"
      :doc "Protocol for network actions"}
  xradar.network)

(defprotocol XRadarNetwork
  "Protocol for network actions"
  (send!
    [this message]
    "Send a message on the active frequency")
  (send-to!
    [this cid message]
    "Send a private message to the provided pilot")
  (update-flightplan
    [this aircraft]
    "Update the provided aircraft's flightplan"))
