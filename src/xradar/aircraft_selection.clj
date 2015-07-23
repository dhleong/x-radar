(ns ^{:author "Daniel Leong"
      :doc "Aircraft selection utils"}
  xradar.aircraft-selection
  (:require [xradar.util :refer [deep-merge]]))

(def first-keys [:j :k :l :h :u :i :o :p :m :n :h])
(def second-keys [:f :d :s :g :a :r :e :w :v :c :x :q :z :b])

(defn- pairs 
  "Generates pairs of keys in a preferred order for selection"
  []
  (for [key-1 first-keys
        key-2 second-keys]
    [key-1 key-2]))

(defn aircraft-to-bindings
  "Generate a binding map for selecting
  the given aircraft"
  [aircraft callback]
  (reduce
    deep-merge
    (map
      (fn [[key-1 key-2] craft]
        {key-1 {key-2 {:call `(~callback ~craft)}}})
      (pairs)
      (keys aircraft))))
