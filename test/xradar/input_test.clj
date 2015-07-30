(ns xradar.input-test
  (:refer-clojure :exclude [empty])
  (:require [clojure.test :refer :all]
            [xradar
             [commands :refer [set-use-native-input!]]
             [input :refer :all]]))

(set-use-native-input! false)

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

(deftest process-press-test
  (testing "Start insert"
    (let [original (empty) 
          state (atom {})
          machine (process-press original {:key :i :key-code 73} state)]
      (is (= :insert (:mode machine)))))
  (testing "Multi-key map"
    (let [original (empty)
          state (atom {})
          machine (process-press original {:key :o :key-code 79} state)]
      (is (contains? (:current-bindings machine) :f))))
  (testing "Insert text"
    (let [original {:mode :insert 
                    :insert-buffer [] 
                    :last-press {:key :h :raw-key \h}}
          state (atom {})
          machine (pressed-in-mode original state)]
      (is (= [\h] (:insert-buffer machine))))))

