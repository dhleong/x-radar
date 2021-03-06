(ns ^{:author "Daniel Leong"
      :doc "Rendering of the textual 'output'"}
  xradar.output
  (:require [clojure.string :refer [lower-case split]]
            [clojure.java.io :as io]
            [clj-time.local :as l]
            [quil.core :as q]
            [xradar
             [radar-util :refer [redraw]]
             [util :refer [object-for with-alpha]]]))

(def output-padding 10)
(def output-size 14)
(def scrollbar-width 8)

;; eg: `[00:00:00] ` but as all spaces
(def multi-line-prefix "           ")

(declare get-active)
(declare build-output)

(defn- prefix-with
  "Prefix an output line as appropriate
  if it is 'with' someone 
  (IE: it's a private message)"
  [line]
  (if-let [with-label (:with-label line)]
    (assoc line 
           :text (str "<" with-label "> " (:text line)))
    line))

(defn- calculate-metrics
  [state]
  (if-let [cached (:output-metrics-cache @state)]
    cached
    (let [active-chat (get-active state)
          text-height (+ (q/text-descent) (q/text-ascent))
          char-width (q/text-width "M")
          base-available-width (- (q/width) 
                                  output-padding output-padding
                                  scrollbar-width)
          available-width (if (= :global active-chat)
                            base-available-width
                            (- base-available-width output-size output-padding)) 
          chars-per-line (int (Math/floor (/ available-width char-width)))
          metrics
          {:text-height text-height
           :char-width char-width
           :base-available-width base-available-width
           :available-width available-width
           :chars-per-line chars-per-line}]
      (swap! state assoc :output-metrics-cache metrics)
      metrics)))

(defn append-output
  "Append a line of output.
  If you provide an id in :with, you MUST
  provide the label to display in :with-label"
  [state text & {:keys [color flag with with-label] :as opts}]
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
      (filter #(or 
                 (= current (:with %))
                 (= :status (:flag %))) 
              raw))))

(defn buffer-count
  [chars-per-line state]
  (count (build-output
           chars-per-line
           (get-active-buffer @state))))

(defn create-output-buffers
  []
  {:output-buffer (atom [])
   :current-output :global
   :pending-messages (atom 0)})

(defn get-active
  "Returns the pilot/controller object of the 
  currently active chat, or :global if viewing 
  global chat."
  [state]
  (let [current (if (map? state)
                  (:current-output state)
                  (:current-output @state))]
    (if (= :global current)
      :global
      (object-for state current))))

(defn invalidate-output!
  "Invalidate any cache"
  [state]
  (swap! state assoc :output-metrics-cache nil))

(defn set-active!
  "Set the currently active chat. Either pass a cid
  of a pilot or controller to filter to, or :global
  to show all chats together"
  [state cid-or-global]
  (invalidate-output! state) ;; metrics change based on active mode
  (swap! state 
         assoc 
         :current-output cid-or-global
         :output-scroll 0)  ; reset the scroll on switch, I guess
  (when (= :global cid-or-global)
    (swap! (:pending-messages @state) (constantly 0))))

(defn format-text
  "Splits text into multiple lines as necessary"
  [chars-per-line line]
  (let [color (:color line)
        lines (partition-all (- chars-per-line 
                                (count multi-line-prefix))
                             (:text line))
        time-text (str "[" (:time line) "] ") 
        time-line (assoc line
                         :text (str time-text
                                    (apply str (first lines))))
        other-lines (rest lines)]
      (-> (cons 
            time-line
            (map
              (fn [text]
                (assoc line 
                       :text (apply str multi-line-prefix text)))
              other-lines))
          reverse)))

;; public mostly for testing
(defn build-output
  [chars-per-line output-buffer]
  (->> output-buffer
       (mapcat format-text 
               (repeat chars-per-line))))

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
      (and 
        (= :global current-output)
        (not (nil? (:with line)))) (-> scheme :output :private)
      (keyword? color) (-> scheme :output color)
      (integer? color) color 
      :else (-> scheme :output :text))))

(defn draw-output
  [state]
  (q/text-size output-size)
  (q/text-align :left)
  (q/rect-mode :corner)
  (let [radar @state
        scheme (-> radar :profile :scheme)
        active-chat (get-active radar)
        pending @(:pending-messages radar)
        max-output-count (-> radar :profile :output-size)
        metrics (calculate-metrics state)
        text-height (:text-height metrics)
        char-width (:char-width metrics)
        base-available-width (:base-available-width metrics)
        available-width (:available-width metrics) 
        chars-per-line (:chars-per-line metrics)
        max-output (* text-height max-output-count)
        upper-left-y (- output-padding max-output)
        ;; output-buffer (get-active-buffer radar)
        output-buffer (build-output
                        chars-per-line
                        (get-active-buffer radar))
        output-scroll (:output-scroll radar)
        ;; scroll bar stuff
        [scroll-start-perc scroll-length-perc] 
        (calculate-scroll 
          max-output-count output-scroll output-buffer)
        scroll-start (* scroll-start-perc max-output)
        scroll-length (* scroll-length-perc max-output)]
    (def lo output-buffer)
    ;; background
    (with-alpha q/fill-int (-> scheme :output :background))
    (q/no-stroke)
    (q/rect output-padding 
            upper-left-y
            base-available-width
            max-output)
    (when (> pending 0)
      (let [pending-text (str pending " pending")]
        (q/push-style)
        (q/with-translation [output-padding upper-left-y]
          (q/stroke-int (-> scheme :output :text))
          (q/line 0
                  0
                  base-available-width
                  0)
          (q/rect 0
                  (- text-height)
                  (+ (q/text-width pending-text)
                     output-padding
                     output-padding)
                  text-height)
          (q/fill-int (-> scheme :output :outgoing))
          (q/text pending-text output-padding (- (q/text-descent))))
        (q/pop-style)))
    (q/push-matrix)
    (when (not= :global active-chat)
      ;; private chat mode!
      (q/with-rotation [(/ Math/PI -2)]
        (q/fill-int (-> scheme :output :text))
        (q/text (or (:callsign active-chat) "???")
                0
                (+ output-padding output-size)))
      ;; offset everything else
      (q/translate (+ output-size output-padding) 0))
    ;; scroll bar!
    (when (> (int scroll-length) 0)
      (q/with-translation [(+ available-width
                              output-padding) 
                           output-padding]
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
    (loop [output (->> output-buffer 
                       (drop output-scroll)
                       (take max-output-count))
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
                 (+ offset output-size)))))
    (q/pop-matrix)))

(defn scroll-output!
  [state amount]
  (let [chars-per-line (:chars-per-line (calculate-metrics state))] 
    (swap! state 
           #(let [outputs (buffer-count chars-per-line state)
                  output-size (-> % :profile :output-size)
                  last-scroll (:output-scroll %)
                  new-scroll (+ amount last-scroll)
                  adjusted (-> new-scroll
                               (min (- outputs output-size))
                               (max 0))]
              (assoc % :output-scroll adjusted)))))
