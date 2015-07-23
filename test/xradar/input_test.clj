(ns xradar.input-test
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
      (is (= original translated))))
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

(deftest pressed-in-mode-test
  (testing "start insert"
    (let [original {:mode :normal :last-press {:key :i}}
          state (atom {})
          machine (pressed-in-mode original state)]
      (is (= :insert (:mode machine))))))

