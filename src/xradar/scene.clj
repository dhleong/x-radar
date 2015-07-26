(ns ^{:author "Daniel Leong"
      :doc "Protocol for drawing a scene"}
  xradar.scene)

(defprotocol XScene
  "Represents scenery, usually backed by
  a file"
  (draw-scene
    [this profile]
    "Draw the elements of the scene as specified
    by the `:draw` list in the `:profile`")
  (get-center
    [this]
    "Get the center coordinate as {:x, :y}
    for this scenery.")
  (get-lon-scale
    [this]
    "Get the longitude scaling")
  (loaded?
    [this]
    "Some scenery implementations may load asynchronously;
    use this to check if it's ready"))
