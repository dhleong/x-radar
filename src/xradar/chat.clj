(ns ^{:author "Daniel Leong"
      :doc "Private chat"}
  xradar.chat
  (:require [clojure.string :refer [lower-case split]]
            [clojure.java.io :as io]
            [quil.core :as q]
            [xradar
             [output :refer [append-output]]
             [network :refer [get-controllers]]
             [util :refer [with-alpha]]]))

(defn- cid-to-controller
  [state cid]
  (let [network (:network @state)
        controllers (get-controllers network)]
    (->> controllers
        (filter #(= cid (:cid %)))
        first)))

(defn object-for
  "Returns the pilot/controller object of the 
  given cid"
  [state cid]
  (get-in @state 
          [:aircraft cid]
          ;; try controller
          (cid-to-controller state cid)))

(defn selected-chat
  "Returns the pilot/controller object of the 
  currently-selected chat, or :global if viewing 
  global chat."
  [state]
  (let [current (:current-output @state)]
    (if (= :global current)
      :global
      (object-for current))))

(defn receive-from
  "Call when a private chat is received"
  [state cid message]
  (append-output state :with cid))
