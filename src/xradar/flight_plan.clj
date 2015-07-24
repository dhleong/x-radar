(ns ^{:author "Daniel Leong"
      :doc "Flight plan editing"}
  xradar.flight-plan
  (:require [seesaw
             [core :as s]
             [mig :refer [mig-panel]]]
            [xradar
             [network :refer [update-flightplan]]
             [radar-util :refer [update-aircraft]]]))

(defmacro field
  [label field-key & opts]
  (let [given-constraint (odd? (count opts))
        constraints (if given-constraint
                      (last opts)
                      "grow,w 70::")
        text-opts (if given-constraint
                    (drop-last opts)
                    opts)
        text-field `(s/text :id ~field-key :text (~field-key ~'craft) ~@text-opts)
        wrapped-text (if (contains? (set opts) :multi-line?)
                       `(s/scrollable (make-tabable ~text-field))
                       text-field)]
    `[[~label "right"]
      [~wrapped-text ~constraints]]))

(defn- make-tabable
  [text]
  ;; credit: http://stackoverflow.com/a/5043957
  (-> text (.setFocusTraversalKeys java.awt.KeyboardFocusManager/FORWARD_TRAVERSAL_KEYS nil))
  (-> text (.setFocusTraversalKeys java.awt.KeyboardFocusManager/BACKWARD_TRAVERSAL_KEYS nil))
  text)

(defn amend-plan
  [state cid new-craft-data]
  ;; (def amending ["yup" state])
  (let [network (:network @state)
        new-craft (assoc new-craft-data :cid cid)]
    (update-aircraft state new-craft)
    (update-flightplan network new-craft)))

(defn amend-plan-handler
  [state cid e]
  (let [frame (s/to-frame e)]
    (amend-plan state cid (s/value frame))))

(def close-action 
  (s/action 
    :name "Close"
    :handler (fn [e] (.dispose (s/to-frame e)))
    :key "menu W"))

(defn save-action
  [state cid]
  (s/action 
    :name "Amend"
    :handler 
    (fn [e] 
      (amend-plan-handler state cid e))))

(defn save-and-close-action
  [state cid]
  (s/action 
    :name "Amend and close"
    :handler 
    (fn [e] 
      (def saving-and-closing "yes")
      (amend-plan-handler state cid e)
      (.dispose (s/to-frame e)))
    :key "menu S"))

(defn open-flight-plan
  "Open the flightplan for the given CID for editing"
  [state cid]
  (let [craft (get-in @state [:aircraft cid])
        scratch-field (field "Scratchpad:" :scratch)
        scratchpad (-> scratch-field last first)]
    (-> (s/frame 
          :title (str "Flight Plan - " (:callsign craft))
          :menubar
          (s/menubar 
            :items [(s/menu :text "File" 
                            :items [close-action 
                                    :separator
                                    (save-action state cid)
                                    (save-and-close-action state cid)])])
          :on-close :dispose
          :resizable? false
          :content
          (mig-panel
            :constraints ["wrap 7"]
            :items 
            (concat
              (field "Callsign:" :callsign :editable? false)
              (field "A/C Type:" :type)
              [["Flight Rules:" "right"]
               [(s/text :text (str (:rules craft))) "grow"]]
              [[(s/button :text "Amend Plan"
                          :listen [:action #(amend-plan-handler state cid %)])
                "grow"]]
              ;
              (field "Depart:" :depart)
              (field "Arrive:" :arrive)
              (field "Alternate" :alternate)
              [[(s/button :text "Refresh Plan") "grow"]]
              ;
              (field "Cruse:" :cruise)
              scratch-field
              (field "Squawk" :squawk)
              [[(s/button :text "Assign Squawk") "grow"]]
              ;
              (field "Route:" :route 
                     :margin 5
                     :multi-line? true
                     :wrap-lines? true
                     :rows 3
                     "grow,span 6")
              (field "Remarks:" :remarks 
                     :margin 5
                     :multi-line? true
                     :wrap-lines? true
                     :rows 2
                     "grow,span 6"))))
        (s/move! :to [100 100])
        s/pack!
        s/show!)
    (s/request-focus! scratchpad)))
