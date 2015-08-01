(ns xradar.output-test
  (:require [clojure.test :refer :all]
            [xradar.output :refer :all]))

(def single-line-entry
  {:text "Test"
   :time "00:00:00"})

(def multi-line-entry 
  {:text "Testing..."
   :time "00:00:00"})

(def multi-line-result
  [{:text "           ng..." :color nil}
   {:text "[00:00:00] Testi" :color nil}])

(def chars-per-line (+ 5 (count "[00:00:00] ")))

(def color-scheme
  {:output
   {:text 0xffEEEEEE
    :outgoing 0xffFFFFFF}})

(deftest format-text-test
  (testing "Format single line"
    (let [formatted (format-text chars-per-line single-line-entry)]
      (is (= [{:text "[00:00:00] Test" :color nil}] formatted))))
  (testing "Format multi-line"
    (let [formatted (format-text chars-per-line multi-line-entry)]
      (is (= multi-line-result formatted)))))

(deftest build-output-test
  (testing "Lines"
    (let [built (build-output 5 chars-per-line [multi-line-entry])]
      (is (= multi-line-result built))))
  (testing "Too many lines"
    (let [built (build-output 1 chars-per-line [multi-line-entry])]
      (is (= [(first multi-line-result)] built)))))

(deftest resolve-color-test
  (testing "Direct colors"
    (is (= 0xffFFFFFF (resolve-color 
                        color-scheme 
                        {:color 0xffFFFFFF}))))
  (testing "Named scheme colors"
    (is (= 0xffEEEEEE (resolve-color 
                        color-scheme 
                        {:color :text})))
    (is (= 0xffFFFFFF (resolve-color 
                        color-scheme 
                        {:color :outgoing})))))
