(ns ^{:author "Daniel Leong"
      :doc "Vatsim network implementation"}
  xradar.networks.vatsim
  (:require [stubby.core :refer [require-stub]]
            [xradar
             [alias :refer [expand-static]]
             [network :refer [XRadarNetwork]]
             [weather :refer [receive-metar!]]
             [util :refer [deep-merge]]]))

(require-stub aileron.core :as a :else xradar.stubs.aileron)


;; FIXME whatever this is. Should probably be
;;  taken from whatever gets returned by get-servers
(def vatsim-port 5432)

(deftype VatsimNetwork [conn controllers-atom]
  XRadarNetwork
  ;; TODO config-voice!
  (connect!
    [this params]
    (let [server-name (:server params)
          host server-name ;; FIXME this
          port vatsim-port]
      (doseq [[k v] params]
        (a/update! conn k v))
      (a/connect! conn host port)))
  (connected?
    [this]
    (a/connected? conn))
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
  (my-callsign
    [this]
    (a/field conn :callsign))
  ;; TODO push-strip!
  (request-atis
    [this airport-icao]
    (a/request-metar conn airport-icao))
  (request-metar
    [this callsign]
    (a/request-atis conn callsign))
  (send!
    [this message]
    (a/transmit! conn message))
  (send-to!
    [this cid message]
    (a/send! conn cid message))
  (stop-request-metar
    [this airport-icao]
    ;; (a/stop-request-metar conn airport-icao)
    (throw (java.lang.UnsupportedOperationException. "Not implemented")))
  (update-flightplan
    [this aircraft]
    (a/config-pilot! conn aircraft)))

(defn create-network
  "Expects an atom that resolves to the state atom"
  [state-atom]
  (let [controllers (atom {})
        conn 
        (a/create-connection
          "xRadar v0.1.0"
          0 1
          "xRadar connection")] 
    (a/update!
      conn
      :atis-factory
      #(let [state @state-atom
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
    (a/listen conn
              :metars
              #(receive-metar! @state-atom %))
    (->VatsimNetwork
     conn
     controllers)))
