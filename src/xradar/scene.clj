(ns ^{:author "Daniel Leong"
      :doc "Protocol for drawing a scene"}
  xradar.scene)

(defprotocol XScene
  "Represents scenery, usually backed by
  a file"
  (draw-scene
    [this profile this-zoom]
    "Draw the elements of the scene as specified
    by the `:draw` list in the `:profile`. The 
    current zoom level `this-zoom` is also provided
    for occlusion purposes (if necessary)")
  (get-center
    [this]
    "Get the center coordinate as {:x, :y}
    for this scenery.")
  (get-magnetic-var
    [this]
    "Get the magnetic variation")
  (get-lon-scale
    [this]
    "Get the longitude scaling")
  (find-point
    [this point-name]
    "Find the named point")
  (loaded?
    [this]
    "Some scenery implementations may load asynchronously;
    use this to check if it's ready"))
