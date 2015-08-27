(ns ^{:author "Daniel Leong"
      :doc "Rendering of the textual 'output'"}
  xradar.output
  (:require [clojure.string :refer [lower-case split]]
            [clojure.java.io :as io]
            [clj-time.local :as l]
            [quil.core :as q]
            [xradar
             [radar-util :refer [redraw]]
             [util :refer [with-alpha]]]))

(def output-padding 10)
(def output-size 14)
(def scrollbar-width 8)

;; eg: `[00:00:00] ` but as all spaces
(def multi-line-prefix "           ")

(defn- prefix-with
  "Prefix an output line as appropriate
  if it is 'with' someone 
  (IE: it's a private message)"
  [line]
  (if-let [with-label (:with-label line)]
    (assoc line 
           :text (str "<" with-label "> " (:text line)))
    line))

(defn append-output
  "Append a line of output.
  If you provide an id in :with, you MUST
  provide the label to display in :with-label"
  [state text & {:keys [color with with-label] :as opts}]
  {:pre [(or (every? nil? [with with-label])
             (every? (complement nil?) [with with-label]))]}
  (let [output (:output-buffer @state)]
    (swap! output 
           (fn [buf new-entry] 
             (cons new-entry buf))
           (assoc opts
                  :text text
                  :time (l/format-local-time 
                          (l/local-now) 
                          :hour-minute-second))))
  (redraw state))

(defn get-active-buffer
  "Return the active buffer as a vector.
  NOTE that it expects the radar MAP, NOT
  the state ATOM (like draw-output). 
  Public for testing purposes; you shouldn't
  need to access the output buffer directly."
  [radar]
  (let [current (:current-output radar)
        raw @(:output-buffer radar)]
    (if (= :global current)
      (map prefix-with raw)
      (filter #(= current (:with %)) raw))))

(defn buffer-count
  [state]
  (count (get-active-buffer @state)))

(defn create-output-buffers
  []
  {:output-buffer (atom [])
   :current-output :global})

(defn set-active!
  "Set the currently active chat. Either pass a cid
  of a pilot or controller to filter to, or :global
  to show all chats together"
  [state cid-or-global]
  (swap! state assoc :current-output cid-or-global))

(defn format-text
  "Splits text into multiple lines as necessary"
  [chars-per-line line]
  (let [color (:color line)
        lines (partition-all (- chars-per-line 
                                (count multi-line-prefix))
                             (:text line))
        time-text (str "[" (:time line) "] ") 
        time-line {:color color
                   :text (str time-text
                              (apply str (first lines)))}
        other-lines (rest lines)]
      (-> (cons 
            time-line
            (map
              (fn [text]
                {:color color
                 :text (apply str multi-line-prefix text)})
              other-lines))
          reverse)))

;; public mostly for testing
(defn build-output
  [max-lines chars-per-line output-buffer]
  (->> output-buffer
       (take max-lines)
       (mapcat format-text 
               (repeat chars-per-line))
       (take max-lines)))

(defn calculate-scroll
  "Returns a tuple of (start, length) describing
  how to draw the scroll bars, where both are a
  percentage of the output height"
  [max-lines scrolled-lines output-buffer]
  (let [buffer-size (count output-buffer)]
    (if (= 0 buffer-size)
    ;; empty buffer is a special case
    [0 0]
    (let [perc (/ max-lines buffer-size)
          offset (/ scrolled-lines buffer-size)]
      (if (>= perc 1)
        ;; more visible lines than we have to render;
        ;;  don't show the bars
        [0 0]
        ;; else, use the calculation
        [offset perc])))))

(defn resolve-color
  "Resolve the color to use to render the line.
  Public mostly for testing"
  [scheme current-output line]
  (let [color (:color line)]
    (cond
      (keyword? color) (-> scheme :output color)
      (integer? color) color 
      (= :global current-output) (-> scheme :output :private)
      :else (-> scheme :output :text))))

(defn draw-output
  [radar]
  (q/text-size output-size)
  (q/text-align :left)
  (q/rect-mode :corner)
  (let [scheme (-> radar :profile :scheme)
        max-output-count (-> radar :profile :output-size)
        max-output (* output-size max-output-count)
        char-width (q/text-width "M")
        available-width (- (q/width) output-padding output-padding)
        chars-per-line (int (Math/floor (/ available-width char-width)))
        output-buffer (get-active-buffer radar)
        output-scroll (:output-scroll radar)
        ;; scroll bar stuff
        [scroll-start-perc scroll-length-perc] 
        (calculate-scroll 
          max-output-count output-scroll output-buffer)
        scroll-start (* scroll-start-perc max-output)
        scroll-length (* scroll-length-perc max-output)]
    ;; background
    (with-alpha q/fill-int (-> scheme :output :background))
    (q/no-stroke)
    (q/rect output-padding 
            (- output-padding
               max-output)
            available-width
            max-output)
    ;; scroll bar!
    (when (> 0 scroll-length)
      (q/with-translation [(- available-width 
                              scrollbar-width) 0]
        (q/no-stroke)
        (q/fill-int (-> scheme :output :text))
        (q/rect 0 (- scroll-start)
                scrollbar-width (- scroll-length))
        (q/no-fill)
        (q/stroke-int (-> scheme :output :text))
        (q/line 0 0 scrollbar-width 0)
        (q/line 0 (- max-output) scrollbar-width (- max-output))))
    ;; output text
    (q/fill-int (-> scheme :output :text))
    (q/no-stroke)
    (loop [output (build-output 
                    max-output-count
                    chars-per-line
                    (drop output-scroll output-buffer))
           offset 0]
      (when (seq output)
        (let [line (first output)]
          (q/fill-int (resolve-color scheme 
                                     (:current-output radar) 
                                     line))
          (q/text (:text line) 
                  output-padding 
                  (- offset)))
        (when (< offset max-output)
          (recur (rest output)
                 (+ offset output-size)))))))
