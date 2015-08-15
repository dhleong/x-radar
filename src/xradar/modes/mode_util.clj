(ns ^{:author "Daniel Leong"
      :doc "Utility methods for radar mode implementations"}
  xradar.modes.mode-util
  (:require [clojure.test :refer [function?]]
            [quil.core :as q]))

(def mapping-offset-x (- 10))
(def mapping-offset-y 0)
(def mapping-text-size 14)

(defn do-draw-aircraft
  "Helper for rendering an aircraft. Translates so the
  origin (0, 0) is on the center of the aircraft, and
  sets the fill and stroke colors to the correct color.
  Also handles drawing the mapping characters for selection,
  and drawing the location histories."
  [radar scheme my-aircraft historical-aircraft my-info-box craft]
  (let [state (:state craft :untracked)
        current-sequence (-> radar :input deref :current-sequence)
        current-sequence-str (->> current-sequence
                                  (map name)
                                  (apply str))
        selected-color (-> scheme :aircraft :selected)
        craft-color (if (:selected craft)
                      selected-color
                      (get-in scheme [:aircraft state]))
        history-color (if (:selected craft)
                        (-> scheme :aircraft :history-selected)
                        (-> scheme :aircraft :history))
        mapping (if (= :select-aircraft (:mode radar))
                  (get-in radar [:craft-bindings (:cid craft)] nil))]
    (q/fill-int craft-color)
    (q/stroke-int craft-color)
    (let [cx (:x craft)
          cy (:y craft)
          x (q/screen-x cx cy 0)
          y (q/screen-y cx cy 0)
          ;; we have to evaluate history eagerly,
          ;;  because we'll be popping the matrix
          ;;  before using it, thus ruining the calculations
          history (doall
                    (map #(let [{:keys [x y]} %]
                            [(q/screen-x x y 0)
                             (q/screen-y x y 0)]) 
                         (:history craft)))
          xp (q/screen-x cx cy 0)
          yp (q/screen-y cx cy 0) ]
      (q/push-matrix)
      (q/camera)
      (q/translate x y 0)
      (q/hint :disable-depth-test) ; so we draw on top of scenery
      ;; base aircraft
      (q/fill-int craft-color)
      (q/stroke-int craft-color)
      (my-aircraft scheme craft)
      ;; info box
      (q/fill-int craft-color)
      (q/stroke-int craft-color)
      (my-info-box scheme craft)
      ;; select-mode mappings
      (when (and mapping
                 (.startsWith (str mapping) current-sequence-str))
        (q/fill-int craft-color)
        (q/text-size mapping-text-size)
        (q/text-align :right :center)
        (q/text (str mapping) mapping-offset-x mapping-offset-y)
        (def the-current-sequence current-sequence)
        (def the-current-sequence-str current-sequence-str)
        (when-not (empty? current-sequence-str)
          (q/fill-int selected-color)
          (q/text (str current-sequence-str " ") mapping-offset-x mapping-offset-y)))
      ;; history
      (when-let [history-seq (seq history)]
        (doseq [[hx hy] history-seq]
          (q/fill-int history-color)
          (q/stroke-int history-color)
          (q/pop-matrix)
          (q/push-matrix)
          (q/camera)
          (q/translate hx hy 0)
          (def last-translate [hx hy])
          (historical-aircraft scheme craft)))
      (q/hint :enable-depth-test)
      (q/pop-matrix))))

