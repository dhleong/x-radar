(ns ^{:author "Daniel Leong"
      :doc "Connection configuration"}
  xradar.connection-config
  (:require [seesaw
             [core :as s]
             [mig :refer [mig-panel]]]
            [xradar
             [network :refer [update-flightplan]]
             [radar-util :refer [update-aircraft]]]))

(def facilities ["Observer"
                 "Flight Service Station"
                 "Clearance/Delivery"
                 "Ground"
                 "Tower"
                 "Approach/Departure"
                 "Center"])
(def ratings ["Observer"
              "Student1"
              "Student2"
              "Student3"
              "Controller1"
              "Controller2"
              "Controller3"
              "Instructor1"
              "Instructor2"
              "Instructor3"
              "Supervisor"
              "Administrator"])

(defn open-connection
  "Open the connection config window"
  [state]
  (let [frame 
        (-> (s/frame
              :title "Connect to Network"
              :on-close :dispose
              :resizable? false
              :content
              (mig-panel
                :constraints ["wrap 4"]
                :items
                [["Callsign:" "Right"]
                 [(s/text :id :callsign) "grow,w 150::"]
                 ["Real Name:" "Right"]
                 [(s/text :id :name) "grow,w 150::"]
                 ; row 2
                 ["Facility:" "Right"]
                 [(s/combobox :id :facility
                              :model facilities) "grow,w 150::"]
                 ["Rating" "Right"]
                 [(s/combobox :id :rating
                              :model ratings) "grow,w 150::"]
                 ; row 3
                 ["Certificate ID:" "Right"]
                 [(s/text :id :cid) "grow,w 150::"]
                 ["Password" "Right"]
                 [(s/password :id :pass) "grow,w 150::"]
                 ]))
            s/pack!
            s/show!)]
    (s/request-focus! (s/select frame [:#callsign]))
    frame))
