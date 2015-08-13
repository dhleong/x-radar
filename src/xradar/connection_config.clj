(ns ^{:author "Daniel Leong"
      :doc "Connection configuration"}
  xradar.connection-config
  (:require [seesaw
             [bind :as b]
             [core :as s]
             [mig :refer [mig-panel]]]
            [xradar
             [network :refer [get-servers]]
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

(def value-fields [:callsign :name :facility 
                   :rating :cid :pass :server :label])
(def text-value-fields [:callsign :name :cid :pass :label])

(defn- connect-action
  [callback]
  (fn [e]
    (callback (s/value (s/to-root e)))
    (.dispose (s/to-root e))))

(defn open-connection
  "Open the connection config window"
  [state callback]
  (let [network (:network @state)
        servers-map (get-servers network)
        frame 
        (s/frame
          :title "Connect to Network"
          :on-close :dispose
          :resizable? false
          :content
          (mig-panel
            :constraints ["wrap 4"]
            :items
            [["Saved Connections" "align 50% 50%,span 4"]
             [(s/scrollable 
                (s/listbox :id :saved
                           :model [])) "grow,span 4 3"]
             ["Callsign:" "Right"]
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
             ;; row 4
             ["Server:", "Right"]
             [(s/combobox :id :server
                          :model (sort (keys servers-map))) "grow"]
             ["Connection Name", "Right"]
             [(s/text :id :label) "grow,w 150::"]
             ;; input
             [(s/button :text "Save Connection" :id :save :enabled? false) "grow"]
             [(s/button :text "Delete Connection" :id :delete :enabled? false) "grow"]
             [(s/button :text "Connect" :id :connect :enabled? false) "grow,span 2"]]))]
    ;; "connect" is only enabled when all the
    ;;  text fields are non-empty. This is also
    ;;  a good condition for "save," but we will
    ;;  add that after we add support for writing
    ;;  profile stuff to disk
    (b/bind 
      (apply b/funnel 
        (->> text-value-fields
             (map 
               #(keyword (str "#" (name %))))
             (map
               #(s/select frame [%]))))
      (b/transform #(every? (complement empty?) %))
      (b/property (s/select frame [:#connect]) :enabled?))
    ;; attach listeners
    (s/listen (s/select frame [:#connect])
              :action (connect-action callback))
    (def last-frame frame)
    (-> frame s/pack! s/show!)
    (s/request-focus! (s/select frame [:#callsign]))))

