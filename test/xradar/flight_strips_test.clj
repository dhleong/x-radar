(ns xradar.flight-strips-test
  (:require [clojure.test :refer :all]
            [xradar.flight-strips :refer :all]))

(defn move
  [bay dir]
  (move-strip-cursor bay dir))

(defn get-col
  [bay col]
  (get @bay col))

(defn slide
  [bay dir & col]
  (move-current-strip bay dir)
  (if col
    (get-col bay col)
    (get-col bay 0)))

(deftest move-cursor-test
  (testing "Boundaries"
    (let [bay (create-strip-bay)]
      (is (= [0 0] (move bay :left)))
      (is (= [0 0] (move bay :up)))
      (is (= [0 0] (move bay :down)))
      ;; there *is* another bay, but
      ;;  since there's nothing in it
      ;;  we can't move the cursor there
      (is (= [0 0] (move bay :right)))))
  (testing "Normal"
    (let [bay (create-strip-bay)]
      (swap! bay assoc 0 ["Foo" "Bar" "Baz"])
      (is (= [0 1] (move bay :down)))
      (is (= [0 2] (move bay :down)))
      (is (= [0 2] (move bay :down)))
      ;; still can't move right
      (is (= [0 2] (move bay :right)))
      (swap! bay assoc 1 ["Buz"])
      ;; when we can move right, we have to
      ;;  jump up to a legal y
      (is (= [1 0] (move bay :right))))))

(deftest move-strip-test
  (testing "Normal"
    (let [bay (create-strip-bay)]
      (swap! bay assoc 0 ["Foo" "Bar"])
      (is (= ["Foo" "Bar"] (get-col bay 0)))
      (is (= ["Bar" "Foo"] (slide bay :down))))))
