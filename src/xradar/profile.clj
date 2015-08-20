(ns ^{:author "Daniel Leong"
      :doc "Profile management"}
  xradar.profile
  (:require [clojure.java.io :refer [file reader]]
            [clojure.test :refer [function?]]
            [xradar
             [bindings :refer [read-bindings]]
             [util :refer [deep-merge]]]))

(def windows? (-> (System/getProperty "os.name")
                  (.toLowerCase)
                  (.contains "windows")))

(def default-settings-file
  (file
    (System/getProperty "user.home")
    (if windows?
     "xradar.settings.edn"
     ".xradar.settings.edn")))

(def default-pro-file
  (file
    (System/getProperty "user.home")
    (if windows?
     "xradarrc.edn"
     ".xradarrc.edn")))

(defn- try-read-bindings
  [input]
  (try
    (read-bindings (reader input))
    (catch java.io.FileNotFoundException e
      ;; not there? just be empty
      {:settings {}
       :bindings {}})))

(defn read-profile
  [reader-or-file & {:keys [settings-reader] 
                     :or [settings-reader try-read-bindings]}]
  {:pre [(function? settings-reader)]}
  (let [profile (try-read-bindings reader-or-file)
        settings-file (or
                        (file (-> profile :settings :settings-file))
                        default-settings-file)
        settings (settings-reader settings-file)]
    (reduce 
      deep-merge 
      [(:settings settings)
       (:settings profile)
       {:bindings (:bindings profile)}])))
