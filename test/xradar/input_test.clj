(ns xradar.input-test
  (:require [clojure.test :refer :all]
            [xradar.input :refer :all]))

(deftest pressed-in-mode-test
  (testing "start insert"
    (let [original {:mode :normal :last-press {:key :i}}
          state (atom {})
          machine (pressed-in-mode original state)]
      (is (= :insert (:mode machine))))))

