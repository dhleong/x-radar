(ns ^{:author "Daniel Leong"
      :doc "Voice Communication Management"}
  xradar.voice-config
  (:require [clojure.string :refer [upper-case]]
            [seesaw
             [core :as s]
             [mig :refer [mig-panel]]
             [table :refer [table-model value-at update-at!]]]
            [xradar
             [network :refer [config-voice! connected?]]]))

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

(defn create-listener
  [network model]
  (reify javax.swing.event.TableModelListener
    (tableChanged [this event]
      (let [e-type (.getType event)
            col (get table-columns (.getColumn event))
            col-key (:key col)
            row (.getFirstRow event)
            row-value (value-at model row)]
        (when (= e-type javax.swing.event.TableModelEvent/UPDATE)
          (cond
            ;; not connected and trying to
            ;;  enable something; we can't change
            ;;  the setting
            (and 
              (= java.lang.Boolean (:class col))
              (true? (get row-value col-key))
              (not (connected? network)))
            (do 
              (s/alert "Not connected" :type :warning)
              (s/invoke-later
                ;; NB: There's a bug in seesaw 1.4.5 that precludes
                ;;  the use of `false` boolean values here....
                #_(update-at! model 
                              row (assoc row-value col-key false))
                ;; ... so do it manually for now
                (.setValueAt model false row (.getColumn event))))
            ;; connected? do it!
            (connected? network)
            (config-voice! network row-value)))))))

(defn open-voice-comms
  "Open the voice communications management window"
  [state]
  (let [network (:network @state)
        model 
        (wrap-model state
          (table-model 
            :columns table-columns
            ;; FIXME load from profile
            :rows [{:name "Test"
                    :freq "121.800"
                    :server "voice.nyartcc.org"
                    :channel "zny_bw"
                    :prim false
                    :rx-t false
                    :tx-t false
                    :rx-v false
                    :tx-v false}]))
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
                  :model (doto model
                          (.addTableModelListener 
                            (create-listener network model))))) 
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
