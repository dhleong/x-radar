(ns xradar.input-test
  (:refer-clojure :exclude [empty])
  (:require [clojure.test :refer :all]
            [xradar.input :refer :all]))

(defn empty [] @(create-input {}))
(defn- with-mods [& mods]
  (let [base (create-input {})]
    (doseq [m mods]
      (process-input-press base {:key m} {}))
    @base))
(defn shifted [] (with-mods :shift))

(deftest translate-event-test
  (testing "Special keys"
    (let [original {:key :esc :key-code 27}
          translated (translate-event (empty) original)]
      (is (= original translated)))
    (let [original {:key-code 59 :raw-key \: :key (keyword ";")}
          translated (translate-event (empty) original)]
      (is (= {:key :colon :key-code 59 :raw-key \:}
             translated))))
  (testing "Simple letters"
    (let [original {:key :l :key-code 76}
          translated (translate-event (empty) original)]
      (is (= original translated))))
  (testing "Shifted letters"
    (let [original {:key :l :key-code 76}
          translated (translate-event (shifted) original)]
      (is (= {:key :L :key-code 76} translated))))
  (testing "ctrl-L"
    (let [original {:key :l :key-code 76}
          translated (translate-event (with-mods :shift :control) original)]
      (is (= {:key :ctrl-L :key-code 76} translated))))
  (testing "alt-cmd-ctrl-L"
    (let [original {:key :l :key-code 76}
          translated (translate-event 
                       (with-mods :command :alt :shift :control) original)]
      (is (= {:key :alt-cmd-ctrl-L :key-code 76} translated)))))

(deftest process-press-test
  (testing "Handle scroll"
    (let [original (with-mods :control) 
          state (atom {:current-output :global
                       :output-buffer (atom [{:text "1"} {:text "2"}
                                             {:text "3"}])
                       :profile {:output-size 1}
                       :output-metrics-cache {:chars-per-line 100}
                       :output-scroll 0})
          machine (process-press original 
                                 {:key :b :key-code 66} 
                                 state)]
      (is (= 1 (:output-scroll @state)))
      ;; we should be where we expect to be
      (is (contains? (:current-bindings machine) :ctrl-b))))
  (testing "Multi-key map"
    (let [original (empty)
          state (atom {})
          machine (process-press original {:key :o :key-code 79} state)]
      (is (contains? (:current-bindings machine) :f)))))

