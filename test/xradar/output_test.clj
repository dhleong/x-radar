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
    :outgoing 0xffFFFFFF
    :private 0xff4DE5FF}})

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

(deftest calculate-scroll-test
  (testing "No scrollbar"
    (let [[start len] (calculate-scroll 
                        4 ;; max-lines
                        0 ;; scrolled distance
                        [])]
      (is (= 0 start len)))
    (let [[start len] (calculate-scroll 
                        4 ;; max-lines
                        0 ;; scrolled distance
                        (repeat 4 "hi"))]
      (is (= 0 start len)))
    (let [[start len] (calculate-scroll 
                        4 ;; max-lines
                        2 ;; scrolled distance
                        (repeat 2 "hi"))]
      (is (= 0 start len))))
  (testing "Scrollbar shown"
    (let [[start len] (calculate-scroll 
                        4 ;; max-lines
                        0 ;; scrolled distance
                        (repeat 5 "hi"))]
      (is (= 0 start))
      (is (= 4/5 len)))
    (let [[start len] (calculate-scroll 
                        4 ;; max-lines
                        1 ;; scrolled distance
                        (repeat 5 "hi"))]
      (is (= 1/5 start)
          (= 4/5 len)))
    (let [[start len] (calculate-scroll 
                        4 ;; max-lines
                        1 ;; scrolled distance
                        (repeat 8 "hi"))]
      (is (= 1/8 start)
          (= 1/2 len)))))

(deftest resolve-color-test
  (testing "Direct colors"
    (is (= 0xffFFFFFF (resolve-color 
                        color-scheme 
                        :global
                        {:color 0xffFFFFFF}))))
  (testing "Named scheme colors"
    (is (= 0xffEEEEEE (resolve-color 
                        color-scheme 
                        :global
                        {:color :text})))
    (is (= 0xffFFFFFF (resolve-color 
                        color-scheme 
                        :global
                        {:color :outgoing}))))
  (testing "Private chats in global mode"
    (is (= 0xff4DE5FF (resolve-color 
                        color-scheme 
                        :global
                        {:with 42}))))
  (testing "Private chats in filtered mode"
    (is (= 0xffEEEEEE (resolve-color 
                        color-scheme 
                        42
                        {:with 42})))))

(deftest buffer-swapping-test
  (let [state (atom (create-output-buffers))]
    (append-output state "Private" :with 42)
    (append-output state "Global")
    (testing "Filter to private with CID"
      (set-active! state 42)
      (is (= 42 (:current-output @state)))
      (is (= 1 (buffer-count state)))
      (is (= "Private" (-> (get-active-buffer @state) first :text))))
    (testing "Include All with :global"
      (set-active! state :global)
      (is (= :global (:current-output @state)))
      (is (= 2 (buffer-count state)))
      (is (= "Global" (-> (get-active-buffer @state) first :text)))
      (is (= "Private" (-> (get-active-buffer @state) second :text))))
    (testing "New CID doesn't break"
      (set-active! state 9001)
      (is (= 9001 (:current-output @state)))
      (is (= 0 (buffer-count state)))
      (is (= [] (get-active-buffer @state))))))
