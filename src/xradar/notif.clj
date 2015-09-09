(ns ^{:author "Daniel Leong"
      :doc "Notification"}
  xradar.notif
  (:require [quil.core :as q]))

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

(defn request-attention!
  "Request attention to the app when not focused"
  [& {:keys [is-critical] :or {is-critical false}}]
  ;; TODO how does windows/linux want to handle this?
  (when-let [app (osx-app)]
    (.requestUserAttention app is-critical)))

;;
;; Artistry
;;

(defn draw-notifs
  "Draws icons representing notification topics"
  [_]
  ;; TODO
  nil)
