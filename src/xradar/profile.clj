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

(defn- pick-settings-file
  [radar]
  (file
    (or (:settings-file (:profile @radar))
        default-settings-file)))

(defn- swap-profile!
  "Swap a new value into a profile field. Only
  updates in-memory."
  [radar field new-value & {:keys [settings-dirty]}]
  (swap! radar deep-merge {:profile {field new-value}
                           :settings-dirty settings-dirty}))

(defn write-profile
  "Write the current settings into the-writer.
  Public for testing"
  [the-writer radar]
  (let [profile (:profile @radar)
        new-settings (select-keys profile settings-keys)]
    (doseq [[k v] new-settings]
      (.write the-writer 
              (str "#set/" (name k) " " v "\n")))))

(defn commit-profile
  "Write the current settings to disk without making
  any other changes (besides clearing the dirty flag)"
  [radar]
  (swap! radar assoc :settings-dirty false)
  (with-open [the-writer (writer (pick-settings-file radar))]
    (write-profile the-writer radar)))

(defn update-profile
  "See write-profile; this handles preparing the writer
  if necessary. By default, it will just mark the profile
  as dirty and swap in the value; if `:commit true` is passed,
  then the dirty flag will be cleared and the updated profile
  will be written to disk"
  [radar field new-value & {:keys [commit]
                            :or {commit false}}]
  (swap-profile! radar field new-value
                 :settings-dirty (not commit))
  (if commit
    (with-open [the-writer (writer (pick-settings-file radar))]
      (write-profile
        the-writer
        radar))
    (do
      (swap-profile! radar field new-value
                     :settings-dirty true))))
