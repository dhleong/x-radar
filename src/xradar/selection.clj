(ns ^{:author "Daniel Leong"
      :doc "Selection utils"}
  xradar.selection
  (:require [xradar.util :refer [deep-merge]]))

(def first-keys [:j :k :l :h :u :i :o :p :m :n :h])
(def second-keys [:f :d :s :g :a :r :e :w :v :c :x :q :z :b])

(defn- pairs 
  "Generates pairs of keys in a preferred order for selection"
  []
  (for [key-1 first-keys
        key-2 second-keys]
    [key-1 key-2]))

(defn from-bindings
  "Given the result of to-bindings,
  return a map of item->binding"
  [bindings]
  (->> bindings
       (mapcat
         (fn [[key-1 choices]]
           (map
             (fn [[key-2 parts]]
               (let [form (-> parts vals last)
                     item (last form)]
                 {item (str (name key-1) (name key-2))}))
             choices)))
       (apply merge)))

(defn to-bindings
  "Generate a binding map for selecting
  the given objects
  `items` is a list"
  [items callback]
  (if-let [seqd (seq items)]
    (reduce
      deep-merge
      (map
        (fn [[key-1 key-2] item]
          {key-1 {key-2 {:call `(~callback ~item)}}})
        (pairs)
        items))
    {}))
