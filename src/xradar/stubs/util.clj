(ns ^{:author "Daniel Leong"
      :doc "Sometimes we can't include the real thing,
           so let's have stubs so our tests can still
           run without them"}
  xradar.stubs.util)

(defmacro defstub
  [fn-name]
  `(defn ~fn-name
     [& ~'args]
     (throw (UnsupportedOperationException. "Stub!"))))

(defmacro require-stub
  "Usage: (require-stub aileron.core :as a :else aileron)"
  [required-ns as the-alias else stub-ns-name]
  `(try
     ;; take care loading aileron so we don't explode
     ;;  when it is not available
     (require '[~required-ns :as ~the-alias])
     (catch java.io.FileNotFoundException ~'e
       (println ~(str "WARN: Using stub for " required-ns))
       (require 'xradar.stubs.aileron)
       (alias (quote ~the-alias) 
              (quote
                ~(symbol (str "xradar.stubs." stub-ns-name)))))))
