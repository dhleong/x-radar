(ns xradar.profile-test
  (:require [clojure.java.io :refer [file]]
            [clojure.string :refer [join]]
            [clojure.test :refer :all]
            [xradar
             [profile :refer :all]])
  (:import [java.io File StringReader StringWriter]))

(def profile
  (join "\n"
        ["#set/settings-file \"my-settings\""
         "#set/draw [:geo :labels]"
         "#map/normal/gc connect"]))

(def empty-file
  {:settings {}
   :bindings {}})

(deftest read-profile-test
  (testing "Profile without settings"
    (let [prof (read-profile 
                 (StringReader. profile)
                 :settings-reader
                 (fn [requested]
                   (is (= (file "my-settings") requested))
                   empty-file))]
      (is (= [:geo :labels] (-> prof :draw)))
      (is (= "my-settings" (-> prof :settings-file)))
      (is (= {:normal {:g {:c {:call 'connect}}}} (-> prof :bindings)))
      ;; nobody specified this
      (is (= nil (-> prof :smoothing)))))
  (testing "Profile with settings"
    (let [prof (read-profile 
                 (StringReader. profile)
                 :settings-reader
                 (fn [requested]
                   (is (= (file "my-settings") requested))
                   {:settings {:draw [:vor]
                               :smoothing 3}}))]
      ;; profile overrides settings
      (is (= [:geo :labels] (-> prof :draw)))
      ;; but settings should be there when no conflict
      (is (= 3 (-> prof :smoothing))))))

(deftest write-profile-test
  (testing "Write new settings"
    (let [radar (atom {})
          output (StringWriter.)]
      (write-profile output radar 
                     :connections [{:callsign "ZNY_ZK_OBS"}])
      (let [conns (-> @radar :profile :connections)
            written (str output)] 
        (is (= "ZNY_ZK_OBS" (:callsign (first conns))))
        (is (= "#set/connections [{:callsign \"ZNY_ZK_OBS\"}]\n"
               written)))))
  (testing "Merge Settings"
    (let [radar (atom {:profile 
                       {:not-a-setting 42
                        :connections [{:callsign "OLD"}]
                        :comms [{:name "LGA_GND" :freq "121.700"}]}})
          output (StringWriter.)]
      (write-profile output radar 
                     :connections [{:callsign "ZNY_ZK_OBS"}])
      (let [conns (-> @radar :profile :connections)
            written (str output)] 
        (is (= 1 (count conns)))
        (is (= "ZNY_ZK_OBS" (:callsign (first conns))))
        (is (= (join
                 ["#set/comms [{:freq \"121.700\", :name \"LGA_GND\"}]\n"
                  "#set/connections [{:callsign \"ZNY_ZK_OBS\"}]\n"])
               written)))))) 
