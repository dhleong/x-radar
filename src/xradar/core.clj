(ns xradar.core
  (:require [clojure.string :refer [upper-case]]
            [seesaw.core :refer [native!]]
            [xradar
             [profile :refer [read-profile]]
             [radar :refer [create-radar]]
             [radar-util :refer [update-aircraft]]
             [sector-scene :as sct]
             [util :refer [resolve-file]]]
            [xradar.networks
             [vatsim :as vatsim]])
  (:gen-class))

;;
;; Global state
;;

(defonce primary-radar (atom nil))

;;
;; Utils
;;

(defn file-extension
  [file-obj]
  (let [file-name (.getName file-obj)
        ext-start (.lastIndexOf file-name ".")]
    (when (not= -1 ext-start)
      (subs file-name (inc ext-start)))))

(defn network-name
  [profile]
  (upper-case (get profile :network "VATSIM")))

(defmacro printerr
  [& body]
  `(binding [*out* *err*]
     (println "ERR:" ~@body)))

;;
;; Component inflation
;;

(defn inflate-profile 
  [args]
  (if-let [pro-file (first args)]
    (do
      (println "Reading profile " pro-file)
      (read-profile pro-file))
    (do
      (println "Reading default profile...")
      (read-profile))))

(defn inflate-network
  [radar-atom profile]
  (let [network (network-name profile)]
    (case network
      "VATSIM" (vatsim/create-network radar-atom)
      (printerr "Unsupported network `" network "`"))))

(defn inflate-scene
  [profile]
  (if-let [scene-file (resolve-file (:scene-file profile))]
    (case (file-extension scene-file)
      "sct" (sct/load-sector scene-file)
      "sct2" (sct/load-sector scene-file)
      (printerr "Unsupported scene file " scene-file))
    (printerr "No scene file specified;"
              "add #set/scene-file \"PATH\" to your profile")))

(defn inflate-voice
  [profile]
  (let [network (network-name profile)]
    (case network
      (printerr "Unsupported voice network `" network "`"))))

;;
;; Main
;;

(defn -main
  "Main entry point to xRadar"
  [& args]
  (native!)
  (let [profile (inflate-profile args)
        network (inflate-network primary-radar profile)
        scene (inflate-scene profile)
        voice (inflate-voice profile)]
    (if (not-any? nil? [profile network scene voice])
      (swap! primary-radar 
             (fn [_]
               (create-radar profile scene network voice)))
      (do
        (println "Error instantiating components; startup aborted")
        (System/exit 1)))))
