(ns ^{:author "Daniel Leong"
      :doc "Default bindings"}
  xradar.bindings
  (:require [clojure
             [edn :as edn]
             [string :refer [split]]]
            [clojure.java.io :as io]
            [xradar
             [commands :as c]
             [util :refer [deep-merge]]]))

(def default-bindings-filename "default-bindings.edn")

(defn anode 
  "Create an action node"
  [fun]
  (if (string? fun)
    {:call #(c/eval-command %1 %2 fun)}
    {:call fun}))

(def default-bindings
  {:normal
   {:i (anode c/start-insert)
    :s (anode c/start-select-aircraft)}

   :select-aircraft
   {:esc (anode c/stop-insert)}
   
   :insert
   {:esc (anode c/stop-insert)
    :default c/handle-insert}})

(defn- read-map
  "Read a key mapping"
  [parts value]
  (let [[_ mode mapping-string] parts
        mapping (map #(-> % str keyword) mapping-string)]
    {(keyword mode) (assoc-in {} mapping {:call value})}))

(defn- read-set
  "Read a setting"
  [parts value]
  (let [[_ setting] parts]
    {(keyword setting) value}))

(defn- read-binding
  [tag value]
  (let [parts (split (str tag) #"/")
        command (first parts)]
    (case command
      "map" {:bindings (read-map parts value)}
      "set" {:settings (read-set parts value)}
      (println "Unsupported command" command))))

(defn read-bindings
  [reader]
  (let [binding-fn read-binding
        input (java.io.PushbackReader. reader)]
    (loop [result {}]
      (let [read-val (edn/read {:default binding-fn
                                :eof :eof} 
                               input)]
        (if (= :eof read-val)
          result
          (recur (deep-merge result read-val)))))))

(defn read-str-bindings
  [string]
  (read-bindings (java.io.StringReader. string)))

(defn read-default-bindings
  []
  (read-bindings 
    (io/reader (io/resource default-bindings-filename))))
