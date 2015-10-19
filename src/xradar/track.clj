(ns ^{:author "Daniel Leong"
      :doc "Aircraft tracking-related. May be largely
           simplified depending on how the network
           provides tracking information"}
  xradar.track
  (:require [xradar.network :as n]))

(defonce tracked-cids #{})

(defn toggle-track
  "Returns true if we're now tracking the cid"
  [state cid]
  (n/toggle-track! (:network @state) cid)
  (let [updated (swap! tracked-cids 
                       #(if (contains? %1 %2)
                          (disj %1 %2)
                          (conj %1 %2))
                       cid)]
    (contains? updated cid)))

(defn tracked?
  "Check if the cid is currently tracked"
  [cid]
  (contains? @tracked-cids cid))
