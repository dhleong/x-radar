(ns ^{:author "Daniel Leong"
      :doc "Voice comms Protocol"}
  xradar.voice)

(defprotocol XVoice
  "Wrap voice communication handling"
  (config!
    [this connection-name config]
    "Configure an active connection")
  (config
    [this connection-name]
    "Retrieve the config for an active connection")
  (connect!
    [this config]
    "Connect with the given config")
  (connections
    [this]
    "Return a sequence of configured 
    connection-names. They may or
    may not be connected.")
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
  (receiving?
    [this connection-name]
    "Check if we're currently receiving on
    the given connection-name")
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
    [this connection-name]
    "Check if we're currently transmitting
    at all, or on the given connection-name
    if provided"))

