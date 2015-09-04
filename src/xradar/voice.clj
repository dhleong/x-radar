(ns ^{:author "Daniel Leong"
      :doc "Voice comms"}
  xradar.voice
  (:require [clojure.string :refer [join lower-case]]))

(defprotocol XVoice
  "Wrap voice communication handling"
  (config!
    [this connection-name config]
    "Configure an active connection")
  (connect!
    [this config]
    "Connect with the given config")
  (connected?
    [this]
    [this connection-name]
    "Check if this is connected with a primary
    frequency (needed to transmit), or at all
    on the given connection-name if provided")
  (disconnect!
    [this] 
    [this connection-name]
    "Disconnect all active connections,
    or the named connection if provided")
  (start-transmitting
    [this]
    "Start recording and transmitting voice 
    on the primary connection")
  (stop-transmitting
    [this]
    "Stop recording and transmitting voice
    on the primary connection")
  (transmitting?
    [this]
    "Check if we're currently transmitting"))
