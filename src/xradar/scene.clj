(ns ^{:author "Daniel Leong"
      :doc "Protocol for drawing a scene"}
  xradar.scene)

(defprotocol XScene
  "Represents scenery, usually backed by
  a file"
  (draw-scene
    [this profile]
    "Draw the elements of the scene as specified
    by the `:draw` list in the `:profile`"))
