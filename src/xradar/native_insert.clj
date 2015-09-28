(ns ^{:author "Daniel Leong"
      :doc "The 'native' insert mode handling"}
  xradar.native-insert
  (:require [clojure.test :refer [function?]]
            [seesaw
             [core :as s]
             [mig :refer [mig-panel]]]
            [xradar.alias :refer [expand-values]])
  (:import [java.awt.event InputEvent]))

(def input-height 30)

(defn- get-cursor
  [input]
  (-> input
      (.getCaret)
      (.getDot)))

(defn select-next-var
  [text & {:keys [from dir] :or {from 0 dir :right}}]
  (let [matcher (re-matcher #"\$[0-9]" text)]
    (case dir
      ;; search forward; easy
      :right
      (cond 
        ;; found a subsequent match!
        (.find matcher from)
        [(.start matcher) (.end matcher)]
        ;; no more; search from the beginning
        (> from 0)
        (select-next-var text)
        ;; otherwise, there are no more matches
        :else nil)
      ;; searching backward is more problematic
      :left
      (let [search-window (subs text 0 from)
            previous-dollar (.lastIndexOf search-window "$")]
        (.reset matcher search-window)
        (cond
          ;; nothing behind us?
          (< previous-dollar 0)
          (if (= (count text) from)
            ;; we already searched the whole string; there's nothing else
            nil
            ;; recurse from the end of the string
            (select-next-var text :from (count text) :dir dir))
          ;; found something there? excellent!
          (.find matcher previous-dollar)
          [(.start matcher) (.end matcher)]
          ;; otherwise, keep looking backwards
          :else (select-next-var text :from previous-dollar :dir dir))))))


(defn- key-handler
  [on-submit on-cancel on-change key-event]
  (def last-event key-event)
  (def last-event-code (.getKeyCode key-event))
  (let [root (s/to-root key-event)
        input (s/select root [:#input])
        key-code (.getKeyCode key-event)
        {:keys [scrollback history]} 
         (s/user-data input)]
    (case key-code
      ;; tab; try to hop between numbered vars
      9 (let [mods (.getModifiers key-event)
              dir (if (= InputEvent/SHIFT_MASK mods)
                    :left
                    :right)
              sel-func (if (= :left dir) first second)
              selection (s/selection input)
              cursor (or (sel-func selection) (get-cursor input))]
          (when-let [next-var (select-next-var
                                (s/value input) 
                                :from cursor
                                :dir dir)]
            (s/selection! input next-var)))
      ;; enter; submit the text
      10 (let [content (.trim (s/value input))]
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

(defn- alias-expander
  [state key-event]
  (let [input (s/to-widget key-event)
        old-value (s/value input)
        expanded (expand-values 
                   state
                   {:cursor (get-cursor input)}
                   old-value)]
    (when (not= old-value expanded)
      (s/value! input expanded)
      (when-let [var-range (select-next-var expanded)]
        (s/selection! input var-range)))))

(defn create-insert
  "Create the insert mode box,
  with the given callbacks"
  [state x y w & {:keys [prompt history 
                         initial initial-selected
                         on-submit on-cancel on-change]
                  :or {initial ""
                       initial-selected false}}]
  {:pre [(function? on-submit) (function? on-cancel)]}
  (let [input 
        (doto
          (s/text
            :id :input
            :user-data {:scrollback (atom -1)
                        :history history}
            :text initial
            :listen 
            [:key-pressed #(try
                             (key-handler on-submit on-cancel on-change %)
                             (catch Exception e
                               (def last-exc e)))
             :key-released #(try
                              (alias-expander state %)
                              (let [key-code (.getKeyCode %)]
                                (when (and (not= 10 key-code)
                                           (not= 27 key-code))
                                  ;; if not a submit or a cancel event, 
                                  ;;  notify the change
                                  (on-change (.trim (s/value %)))))
                              (catch Exception e
                                (def last-exc e)))])
          (.setFocusTraversalKeysEnabled false))
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
    (s/request-focus! input)
    (when initial-selected
      (s/selection! input [0 (count initial)]))
    ;; return the frame
    window))
