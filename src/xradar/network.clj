(ns ^{:author "Daniel Leong"
      :doc "Protocol for network actions"}
  xradar.network)

(defprotocol XRadarNetwork
  "Protocol for network actions"
  (update-flightplan
    [this aircraft]
    "Update the provided aircraft's flightplan"))
