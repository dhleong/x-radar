(ns ^{:author "Daniel Leong"
      :doc "Voice Communication Management"}
  xradar.voice-config
  (:require [clojure.string :refer [upper-case]]
            [seesaw
             [bind :as b]
             [core :as s]
             [mig :refer [mig-panel]]
             [table :refer [insert-at! remove-at! table-model update-at! value-at]]]
            [xradar
             [network :refer [config-voice! connected?]]
             [profile :refer [update-profile]]
             [util :refer [list-replace when-none-empty-set-enabled]]]))

(defn as-box
  [the-key]
  {:key the-key
   :text (upper-case (name the-key))
   :class java.lang.Boolean})

(def text-value-fields [:name :freq :server :channel])

(def table-columns
  (concat
    text-value-fields
    [(as-box :prim) (as-box :rx-t) (as-box :tx-t)
     (as-box :rx-v) (as-box :tx-v)]))
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

(defn- save-action
  [radar]
  (fn [e]
    (let [frame (s/to-frame e)
          table (s/select frame [:#items])
          old-connections (get (:profile @radar) :voice [])
          selected-index (s/selection table)
          selection (if (nil? selected-index)
                      nil
                      (nth old-connections selected-index))
          conn-value (select-keys (s/value frame) text-value-fields)
          new-connections (if selection
                            (list-replace selection conn-value old-connections)
                            (cons conn-value old-connections))]
      (update-profile radar :voice new-connections)
      (when-not selection
        (s/value! frame (zipmap text-value-fields
                                (repeat ""))))
      (if selection
        ;; update selected
        (update-at! table selected-index conn-value)
        ;; none selected; create new
        (insert-at! table 0 conn-value)))))

(defn- delete-action
  [radar]
  (fn [e]
    (let [frame (s/to-frame e)
          table (s/select frame [:#items])
          old-connections (get (:profile @radar) :voice [])
          selected-index (s/selection table)
          selection (nth old-connections selected-index)
          new-connections (vec (remove (partial = selection) old-connections))]
      (update-profile radar :voice new-connections)
      (try
        (remove-at! table selected-index)
        (catch NullPointerException e
          ;; FIXME Hacks! An NPE is thrown in remove-at!
          ;;  without any stack trace. As far as I can tell,
          ;;  everything passed to remove-at! is fine (and
          ;;  indeed, the row *is* removed from the table;
          ;;  the UI is just not repainted, hence below).
          ;;  Seesaw's unit tests for remove-at! have no
          ;;  problems, so it must be something we're doing.
          ;;  For now, though, this works.
          ;; (def last-exc e)
          ;; (clojure.stacktrace/print-stack-trace last-exc)
          (s/repaint! frame)))
      (s/selection! table nil))))

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
  (let [radar @state
        network (:network radar)
        model 
        (wrap-model state
          (table-model 
            :columns table-columns
            :rows (get-in radar [:profile :voice] [])))
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
             [""] ;; spacing
             [(s/button :text "Delete" :id :delete :enabled? false) "grow,span 4"]
             [(s/button :text "Save" :id :save :enabled? false) "grow,span 4"]]))]
    (-> frame s/pack! s/show!)
    (s/config! (s/select frame [:#items])
               :column-widths table-column-widths)
    ;; some bindings
    (when-none-empty-set-enabled
      (s/select frame [:#save])
      text-value-fields)
    (b/bind
      (b/selection (s/select frame [:#items]))
      (b/transform #(nth (-> @state :profile :voice) %))
      (b/transform select-keys text-value-fields)
      (b/tee 
        (b/value frame)
        (b/property (s/select frame [:#delete]) :enabled?)))
    ;; attach listeners
    (s/listen (s/select frame [:#save])
              :action (save-action state))
    (s/listen (s/select frame [:#delete])
              :action (delete-action state))
    ;; return the frame
    (def last-frame frame)
    frame))
