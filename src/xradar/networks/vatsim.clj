(ns ^{:author "Daniel Leong"
      :doc "Vatsim network implementation"}
  xradar.networks.vatsim
  (:require [xradar
             [alias :refer [expand-static]]
             [network :refer [XRadarNetwork]]
             [util :refer [deep-merge]]]
            [xradar.stubs.util :refer [require-stub]]))

(require-stub aileron.core :as a :else aileron)


;; FIXME whatever this is. Should probably be
;;  taken from whatever gets returned by get-servers
(def vatsim-port 5432)

(deftype VatsimNetwork [conn controllers-atom]
  XRadarNetwork
  ;; TODO config-voice!, connected?
  (connect!
    [this params]
    (let [server-name (:server params)
          host server-name ;; FIXME this
          port vatsim-port]
      (doseq [[k v] params]
        (a/update! conn k v))
      (a/connect! conn host port)))
  (disconnect!
    [this]
    (a/disconnect! conn))
  (get-controllers
    [this]
    (vals @controllers-atom))
  (get-servers
    [this]
    ;; TODO use real info; fetch at some point
    {"USA-E" 
     {:ip "127.0.0.1"
      :location "New York, USA"}})
  ;; TODO my-callsign, push-strip!
  (send!
    [this message]
    (a/transmit! conn message))
  (send-to!
    [this cid message]
    (a/send! conn cid message))
  ;; TODO update-flightplan
  )

(defn create-network
  "Expects an atom that resolves to the state atom"
  [radar-atom]
  (let [controllers (atom {})
        conn 
        (a/create-connection
          "xRadar v0.1.0"
          0 1
          "xRadar connection")] 
    (a/update!
      conn
      :atis-factory
      #(let [state @radar-atom
             raw-atis (-> @state :profile :atis)]
         (map
           (partial expand-static state)
           raw-atis)))
    ;; TODO remove controllers on leave
    (a/listen conn
              :controllers 
              #(swap!
                 controllers 
                 deep-merge
                 {(:callsign %) %}))
    (->VatsimNetwork
     conn
     controllers)))
