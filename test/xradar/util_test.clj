(ns xradar.util-test
  (:require [clojure.test :refer :all]
            [xradar.util :refer :all]))

(deftest with-alpha-test
  (testing "No alpha specified"
    (let [result (with-alpha list 0xEEEEEE)]
      (is (= 1 (count result)))))
  (testing "Alpha specified"
    (let [result (with-alpha list 0xddEEEEEE)]
      (is (= 2 (count result)))
      (is (= 0xddEEEEEE (first result)))
      (is (= 221 (second result))))))

(deftest list-replace-test
  (testing "Replace primitives"
    (is (= [1 2 3 8] (list-replace 4 8 [1 2 3 4]))))
  (testing "Replace maps"
    (is (= [{:win 4} {:bar 2} {:baz 3}]
           (list-replace {:foo 1} {:win 4} 
                         [{:foo 1} {:bar 2} {:baz 3}])))))

(deftest set-toggle-test
  (testing "Was missing"
    (is (= #{1 2 3}
           (set-toggle #{1 2} 3))))
  (testing "Was present"
    (is (= #{1 2}
           (set-toggle #{1 2 3} 3)))))

(deftest bearing-test
  (testing "Bearing"
    (is (= (/ Math/PI 4)
           (bearing-to {:x 2 :y 2} {:x 4 :y 4})))
    (is (= (/ Math/PI 2)
           (bearing-to {:x 2 :y 2} {:x 2 :y 4})))
    (is (= Math/PI
           (bearing-to {:x 2 :y 2} {:x 0 :y 2})))
    (is (= (* (/ Math/PI 4) -3)
           (bearing-to {:x 4 :y 4} {:x 2 :y 2})))))
