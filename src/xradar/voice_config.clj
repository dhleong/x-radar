(ns ^{:author "Daniel Leong"
      :doc "Voice Communication Management"}
  xradar.voice-config
  (:require [clojure.string :refer [upper-case]]
            [seesaw
             [core :as s]
             [mig :refer [mig-panel]]
             [table :refer [table-model]]]
            [xradar
             [network :refer [connected?]]]))

(defn as-box
  [the-key]
  {:key the-key
   :text (upper-case (name the-key))
   :class java.lang.Boolean})

(def table-columns
  [:name :freq :server :channel (as-box :prim) 
   (as-box :rx-t) (as-box :tx-t) (as-box :rx-v) (as-box :tx-v)])
(def first-box-column 4)

(def table-column-widths
  [140 80 180 100
   40 40 40 40 40])

(defn- wrap-model
  [state model]
  (update-proxy
    model
    {"isCellEditable" (fn [this row col]
                        (>= col first-box-column))}))

(defn open-voice-comms
  "Open the voice communications management window"
  [state]
  (let [model 
        (wrap-model state
          (table-model 
            :columns table-columns
            :rows [{:name "Test"
                    :freq "121.800"
                    :server "voice.nyartcc.org"
                    :channel "zny_bw"
                    :prim true
                    :rx-t true
                    :tx-t true
                    :rx-v true
                    :tx-v true}]))
        frame
        (s/frame
          :title "Voice Communications"
          :on-close :dispose
          :resizable? false
          :content
          (mig-panel
            :constraints ["wrap 8"]
            :items
            [[(s/scrollable
                (s/table
                  :id :items
                  :column-widths table-column-widths
                  :model model)) 
              "grow,span 8 2,w 700::"]
             ;; inputs
             ["Name:" "Right"]
             [(s/text :id :name) "grow,w 140::"]
             ["Freq:" "Right"]
             [(s/text :id :freq) "grow,w 80::"]
             ["Server:" "Right"]
             [(s/text :id :server) "grow,w 180::"]
             ["Channel:" "Right"]
             [(s/text :id :channel) "grow,w 100::"]
             ;; buttons
             ;; TODO
             ]))]
    (-> frame s/pack! s/show!)
    (s/config! (s/select frame [:#items])
               :column-widths table-column-widths)))
