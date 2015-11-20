(ns ^{:author "Daniel Leong"
      :doc "Aircraft tracking-related. May be largely
           simplified depending on how the network
           provides tracking information"}
  xradar.track
  (:require [xradar
             [network :as n]
             [util :refer [set-toggle]]]))

(defonce tracked-cids #{})

(defn toggle-track
  "Returns true if we're now tracking the cid"
  [state cid]
  (let [network (:network @state)
        updated (swap! tracked-cids 
                       set-toggle
                       cid)
        now-tracked? (contains? updated cid)]
    (if now-tracked?
      (n/track! network cid)
      (n/track-drop! network cid))
    ;; return the tracked status
    now-tracked?))

(defn tracked?
  "Check if the cid is currently tracked"
  [cid]
  (contains? @tracked-cids cid))
