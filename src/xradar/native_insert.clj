(ns ^{:author "Daniel Leong"
      :doc "The 'native' insert mode handling"}
  xradar.native-insert
  (:require [clojure.test :refer [function?]]
            [seesaw
             [core :as s]
             [mig :refer [mig-panel]]]))

(def input-height 20)

(defn- key-handler
  [on-submit on-cancel key-event]
  (def last-event key-event)
  (def last-event-code (.getKeyCode key-event))
  (case (.getKeyCode key-event)
    ;; enter; submit the text
    10 (let [content (s/value (s/to-widget key-event))]
         (-> (s/to-root key-event)
             s/dispose!)
         (on-submit content))
    ;; esc; dispose the window
    27 (do
         (-> (s/to-root key-event)
             s/dispose!)
         (on-cancel))
    ;; anything else, do nothing
    false))

(defn create-insert
  "Create the insert mode box,
  with the given callbacks"
  [x y w & {:keys [on-submit on-cancel]}]
  {:pre [(function? on-submit) (function? on-cancel)]}
  (let [window 
        (-> (s/frame
              :id :insert-box
              :resizable? false
              :undecorated? true
              :on-close :dispose
              :content
              (s/text 
                :id :input
                  :preferred-size [w :by input-height]
                :listen 
                [:key-pressed #(key-handler on-submit on-cancel %)]))
            (s/move! :to [x y])
            s/pack!
            s/show!)]
    (s/request-focus! (s/select window [:#input]))
    ;; return the frame
    window))
