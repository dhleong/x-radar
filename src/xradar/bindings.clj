(ns ^{:author "Daniel Leong"
      :doc "Default bindings"}
  xradar.bindings
  (:require [clojure
             [edn :as edn]
             [string :refer [split]]]
            [clojure.java.io :as io]
            [xradar
             [util :refer [deep-merge]]]))

(def default-bindings-filename "default-bindings.edn")

(defn- read-map
  "Read a key mapping"
  [parts value]
  (let [[_ mode mapping-string] parts
        mapping-parts (re-seq #"(<([^>]+)>|\w|[+-_=;:,./?'\"])" mapping-string)
        mapping (map (fn [[_ simple special]]
                       (keyword 
                         (if (nil? special)
                           simple
                           special)))
                     mapping-parts)]
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
  (with-open [reader (io/reader 
                       (io/resource default-bindings-filename))]
    (read-bindings reader)))
