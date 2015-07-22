(ns ^{:author "Daniel Leong"
      :doc "Default bindings"}
  xradar.bindings
  (:require [xradar.commands :as c]))

(defn anode 
  "Create an action node"
  [fun]
  (if (string? fun)
    {:call #(c/eval-command %1 %2 fun)}
    {:call fun}))

(def default-bindings
  {:normal
   {:i (anode c/start-insert)
    :s (anode c/start-select-aircraft)}

   :select-aircraft
   {:esc (anode c/stop-insert)}
   
   :insert
   {:esc (anode c/stop-insert)
    :default c/handle-insert}})
