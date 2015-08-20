(ns xradar.profile-test
  (:require [clojure.java.io :refer [file]]
            [clojure.string :refer [join]]
            [clojure.test :refer :all]
            [xradar
             [profile :refer :all]])
  (:import [java.io File StringReader]))

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
