(ns ^{:author "Daniel Leong"
      :doc "The 'native' insert mode handling"}
  xradar.native-insert
  (:require [clojure.test :refer [function?]]
            [seesaw
             [core :as s]
             [mig :refer [mig-panel]]]
            [xradar.alias :refer [expand-values]]))

(def input-height 30)

(defn- key-handler
  [on-submit on-cancel key-event]
  (def last-event key-event)
  (def last-event-code (.getKeyCode key-event))
  (let [root (s/to-root key-event)
        input (s/select root [:#input])
        {:keys [scrollback history]} 
         (s/user-data input)]
    (case (.getKeyCode key-event)
      ;; enter; submit the text
      10 (let [content (.trim (s/value (s/to-widget key-event)))]
           (-> root
               s/dispose!)
           (if (empty? content)
             (on-cancel)
             (on-submit content)))
      ;; esc; dispose the window
      27 (do
           (-> root
               s/dispose!)
           (on-cancel))
      ;; up-arrow; history scroll-back
      38 (when (and
                 (seq history)
                 (< (inc @scrollback) (count history)))
           (let [new-idx (swap! scrollback inc)]
             (s/value! input (nth history new-idx))))
      ;; down-arrow; history scroll-forward
      40 (when (and
                 (seq history)
                 (>= @scrollback 0))
           (let [new-idx (swap! scrollback dec)]
             (def last-idx new-idx)
             (s/value! input 
                       (if (< new-idx 0)
                         ""
                         (nth history new-idx)))))
      ;; anything else, do nothing
      false)))

(defn select-next-var
  [text & {:keys [from dir] :or {from 0 dir 1}}]
  ;; TODO support searching backwards, looping around
  (let [matcher (re-matcher #"\$[0-9]" text)]
    (when (.find matcher from)
      [(.start matcher) (.end matcher)])))

(defn- alias-expander
  [state key-event]
  (let [input (s/to-widget key-event)
        old-value (s/value input)
        expanded (expand-values 
                   state
                   {:cursor (-> input
                                (.getCaret)
                                (.getDot))}
                   old-value)]
    (when (not= old-value expanded)
      (s/value! input expanded)
      (when-let [var-range (select-next-var expanded)]
        (s/selection! input var-range)))))

(defn create-insert
  "Create the insert mode box,
  with the given callbacks"
  [state x y w & {:keys [prompt history on-submit on-cancel]}]
  {:pre [(function? on-submit) (function? on-cancel)]}
  (let [input 
        (s/text 
          :id :input
          :user-data {:scrollback (atom -1)
                      :history history}
          :listen 
          [:key-pressed #(try
                           (key-handler on-submit on-cancel %)
                           (catch Exception e
                             (def last-exc e)))
           :key-released #(try
                            (alias-expander state %)
                            (catch Exception e
                              (def last-exc e)))])
        contents
        (if (string? prompt)
          (mig-panel
            :preferred-size [w :by input-height]
            :constraints ["ins 0" "[][grow]" "[shrink 0]"]
            :items
            [[prompt "right"]
             [input "grow"]])
          (s/config! input :preferred-size [w :by input-height]))
        window 
        (-> (s/frame
              :id :insert-box
              :resizable? false
              :undecorated? true
              :on-close :dispose
              :content contents)
            (s/move! :to [x y])
            s/pack!
            s/show!)]
    (s/request-focus! (s/select window [:#input]))
    ;; return the frame
    window))
