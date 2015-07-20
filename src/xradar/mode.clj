(ns ^{:author "Daniel Leong"
      :doc "Radar Mode protocol"}
  xradar.mode)

(defprotocol RadarMode
  (draw-aircraft
    [this scheme aircraft] 
    "Draw the aircraft using the given colorscheme"))
