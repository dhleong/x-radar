(ns ^{:author "Daniel Leong"
      :doc "Notification"}
  xradar.notif
  (:require [clojure
             [set :refer [union]]
             [string :refer [upper-case]]]
            [quil.core :as q]
            [xradar.util :refer [in-focus? with-alpha]]))

;;
;; Constants
;;

(def text-size 10)
(def padding 8)

;;
;; Global state
;;

(defonce critical (atom #{}))
(defonce requested (atom #{}))

;;
;; Utils
;;

(defn osx-app []
  (try
    (let [app-class (Class/forName "com.apple.eawt.Application")
          getter (.getMethod app-class "getApplication" (into-array Class []))]
      (.invoke getter nil (into-array [])))
    (catch Exception e
      ;; not OSX, or OSX extensions missing
      (println e)
      nil)))

;;
;; Public methods
;;

(defn ack-attention!
  "Acknowledge attention request. If
  topic is not provided, will clear
  all non-critical requests."
  ([]
   (swap! requested (constantly #{})))
  ([topic]
   (swap! requested disj topic)
   (swap! critical disj topic)))

(defn request-attention!
  "Request attention to the app when not focused"
  [& {:keys [is-critical topic]
      :or {is-critical false}}]
  (when topic
    (cond
      is-critical 
      (swap! critical conj topic)
      (not (in-focus?))
      (swap! requested conj topic)))
  ;; TODO how does windows/linux want to handle this?
  (when-let [app (osx-app)]
    (.requestUserAttention app is-critical)))

(defn requested-topics
  "Return a set of all the requested attention topics"
  []
  (union
    @critical
    @requested))

;;
;; Artistry
;;

(defn draw-notifs
  "Draws icons representing notification topics"
  [radar]
  (q/text-size text-size)
  (let [scheme (-> radar :profile :scheme)
        height (+ (q/text-ascent)
                  (q/text-descent))
        width (+ (q/text-width "M")
                 padding
                 padding)]
    (doseq [entry (map
                    #(-> (name %)
                         (subs 0 1)
                         upper-case)
                    (sort (requested-topics)))]
      ;; bg
      (q/no-stroke)
      (with-alpha q/fill-int (-> scheme :output :background-highlight))
      (q/rect-mode :corner)
      (q/rect 0 0 width height)
      ;; border
      (q/stroke-int (-> scheme :output :outgoing))
      (q/no-fill)
      (q/rect 0 0 width height)
      ;; body
      (q/fill-int (-> scheme :output :outgoing))
      (q/text entry padding text-size)
      (q/translate [width 0]))))
