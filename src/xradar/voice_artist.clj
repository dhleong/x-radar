(ns ^{:author "Daniel Leong"
      :doc "Drawing voice comms top bar stuff"}
  xradar.voice-artist
  (:require [quil.core :as q]
            [xradar
             [util :refer [with-alpha]]
             [voice :as v]]))

;;
;; Constants
;;

(def text-size 10)

;;
;; Artistry
;;

(defn draw-voice
  "Render active voice comms"
  [radar]
  (q/text-size text-size)
  (q/rect-mode :corner)
  (let [voice (:voice radar)
        scheme (-> radar :profile :scheme)
        char-width (q/text-width "M")
        height (+ (q/text-ascent)
                  (q/text-descent))]
    (doseq [cname (v/connections voice)]
      (let [r? (v/receiving? voice cname)
            t? (v/transmitting? voice cname)
            info (v/config voice cname)
            freq (if (v/connected? voice cname)
                   (:freq info)
                   "---.---")
            cname-width (* char-width (count cname))
            freq-width (* char-width (count freq))]
        ;; receive bg
        (with-alpha q/fill-int 
          (if r? 
            (-> scheme :voice :receive)
            (-> scheme :output :background)))
        (q/rect 0 0 cname-width height)
        ;; conn name
        (q/fill-int (-> scheme :output :outgoing))
        (q/text cname 0 text-size)
        (q/translate [(+ char-width cname-width) 0])
        ;; transmit bg
        (with-alpha q/fill-int 
          (if t? 
            (-> scheme :voice :transmit)
            (-> scheme :output :background)))
        (q/rect 0 0 freq-width height)
        ;; conn freq
        (q/fill-int (-> scheme :output :outgoing))
        (q/text freq 0 text-size)
        (q/translate [freq-width 0])))))

