(ns xradar.bindings-test
  (:require [clojure.test :refer :all]
            [xradar
             [bindings :refer :all]]))

(defn parse
  [string]
  (:bindings (read-str-bindings string)))

(deftest read-bindings-test
  (testing "Single key mode map"
    (let [bindings (parse "#map/normal/i start-insert")]
      (is (= {:normal {:i {:call 'start-insert}}} bindings)))))
