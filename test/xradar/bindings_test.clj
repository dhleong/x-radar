(ns xradar.bindings-test
  (:require [clojure.test :refer :all]
            [xradar
             [bindings :refer :all]]))

(defn bindings
  [string]
  (:bindings (read-str-bindings string)))

(defn settings
  [string]
  (:settings (read-str-bindings string)))

(deftest read-bindings-test
  (testing "Single key mode map"
    (let [bindings (bindings "#map/normal/i start-insert")]
      (is (= {:normal {:i {:call 'start-insert}}} bindings))))
  (testing "Multi-key mode map"
    (let [bindings (bindings "#map/normal/gi start-insert")]
      (is (= {:normal {:g {:i {:call 'start-insert}}}} bindings))))
  (testing "Settings"
    (let [bindings (settings "#set/timeout-len 500")]
      (is (= {:timeout-len 500} bindings)))))
