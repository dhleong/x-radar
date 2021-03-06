(ns ^{:author "Daniel Leong"
      :doc "Generic selection mode"}
  xradar.selection-mode
  (:require [clojure.string :refer [join lower-case]]
            [quil.core :as q]
            [xradar
             [selection :refer [to-bindings from-bindings]]
             [util :refer [deep-merge with-alpha]]]))

(def text-size 14)
(def text-padding 8)
(def separator "  ")

(def not-nil? (complement nil?))

(defn create-lines
  "Given a bunch of item->bindings, generate
  lines of text to represent them"
  [available-width char-width to-string bindings]
  (->>  bindings
       (map (fn [[item mapping]]
              (str "[" mapping "]" (to-string item))))
       (reduce
         (fn [lines part]
           (let [last-line (first lines)
                 last-len (count last-line)]
             (if (<= (+ last-len
                        (count separator)
                        (count part))
                     available-width)
               ;; add the item to the current line
               (cons (if last-line
                       (str last-line separator part)
                       part) (rest lines))
               ;; create a new line
               (cons part lines))))
         ;; start with an empty vector
         [])))

(defn render-selections
  [radar]
  (q/text-align :center :center)
  (q/text-size text-size)
  (let [w (q/width)
        h (q/height)
        center-x (/ w 2)
        center-y (/ h 2)
        scheme (-> radar :profile :scheme)
        bindings (:select-bindings radar)
        to-string (:select-to-string radar)
        char-width (q/text-width "M")
        lines (create-lines (- w text-padding text-padding)
                            char-width to-string bindings)
        with-prompt (if-let [prompt (:select-prompt radar)]
                      (cons prompt lines)
                      lines)
        radius (* text-size (count with-prompt))
        first-line (+ text-size (- center-y radius))]
    (q/rect-mode :radius)
    (q/stroke-int (-> scheme :output :text))
    (with-alpha q/fill-int (-> scheme :output :background))
    (q/rect center-x center-y center-x radius)
    (q/fill-int (-> scheme :output :text))
    (if (seq lines)
      (loop [lines with-prompt
             offset first-line]
        (when-let [line (first lines)]
          (q/text line center-x offset)
          (recur (rest lines)
                 (+ offset text-size))))
      (q/text "No matches" center-x first-line))))

(defn callback
  "Retrieve the callback symbol passed to
  start most-recently, if any"
  [state]
  (:select-callback @state))

(defn start
  "Start selection mode.
  on-cancel and on-select MUST
  update the mode, or you'll get stuck.
  If to-string is provided, it will be used
  to generate the string values to display
  for each item."
  [machine state & {:keys [items
                           on-cancel on-select
                           prompt to-string
                           require-items]
                    :or {require-items true}}]
  {:pre [(not-nil? on-cancel)
         (not-nil? on-select)
         (or (false? require-items) (seq items))]}
  (let [bindings (to-bindings items on-select)
        string-pred (or to-string str)]
    (swap! state 
           #(assoc %
                   :select-bindings (from-bindings bindings)
                   :select-callback on-select
                   :select-to-string string-pred
                   :select-prompt prompt))
    (try
      (def last-bindings (assoc bindings
                                :esc {:call on-cancel}))
      (def last-m machine)
      (catch Exception e
        (def last-e e)))
    (assoc machine
           :mode :select
           :current-bindings 
           (assoc bindings
                  :esc {:call on-cancel}))))
