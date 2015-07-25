(ns xradar.sector-scene-test
  (:require [clojure.test :refer :all]
            [xradar.sector-scene :refer :all]))

(defn load-data
  [string]
  (load-sector-data (java.io.StringReader. string)))

(deftest utils-tests
  (testing "parse-color"
    (is (= 0xffFF0000 (parse-color "255")))
    (is (= 0xff00FF00 (parse-color "65280")))
    (is (= 0xff0000FF (parse-color "16711680")))
    ;; colors from ZNY
    (is (= 0xff606060 (parse-color "6316128")))
    (is (= 0xff142A00 (parse-color "10772")))))

(deftest load-sector-test
  (testing "#define"
    (let [data (load-data "#define Foo 255")]
      (is (= 0xffFF0000 (-> data :colors :Foo))))))
