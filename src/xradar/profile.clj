(ns ^{:author "Daniel Leong"
      :doc "Profile management"}
  xradar.profile
  (:require [clojure.java.io :refer [file reader writer]]
            [clojure.test :refer [function?]]
            [xradar
             [bindings :refer [read-bindings]]
             [util :refer [deep-merge]]]))

(def settings-keys [:connections :comms])

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
  "Public for testing; read the profile and settings
  and merge as appropriate. If no reader or file is
  provided, it will read the default profile."
  ([]
   (read-profile default-pro-file))
  ([reader-or-file & {:keys [settings-reader] 
                      :or {settings-reader try-read-bindings}}]
   ;; {:pre [(function? settings-reader)]}
   (def fun? settings-reader)
   (let [profile (try-read-bindings reader-or-file)
         settings-file (or
                         (file (-> profile :settings :settings-file))
                         default-settings-file)
         settings (settings-reader settings-file)]
     (reduce 
       deep-merge 
       [(:settings settings)
        (:settings profile)
        {:bindings (:bindings profile)}]))))

(defn write-profile
  "Public for testing; swap the new-value into the
  profile in the radar atom and write into the writer"
  [the-writer radar field new-value]
  (swap! radar deep-merge {:profile {field new-value}})
  (let [profile (:profile @radar)
        new-settings (select-keys profile settings-keys)]
    (doseq [[k v] new-settings]
      (.write the-writer 
              (str "#set/" (name k) " " v "\n")))))

(defn update-profile
  "See write-profile; this handles preparing the writer"
  [radar field new-value]
  (let [profile (:profile @radar)
        settings-file (file
                        (or (:settings-file profile)
                            default-settings-file))]
    (with-open [the-writer (writer settings-file)]
      (write-profile
        the-writer
        radar
        field
        new-value))))
