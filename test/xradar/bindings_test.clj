(ns xradar.bindings-test
  (:require [clojure.test :refer :all]
            [xradar
             [bindings :refer :all]]))

(defn aliases
  [string]
  (:aliases (read-str-bindings string)))

(defn bindings
  [string]
  (:bindings (read-str-bindings string)))

(defn settings
  [string]
  (:settings (read-str-bindings string)))

(deftest read-bindings-test
  (testing "Single key mode map"
    (let [result (bindings "#map/normal/i start-insert")]
      (is (= {:normal {:i {:call 'start-insert}}} result))))
  (testing "Single special key mode map"
    (let [result (bindings "#map/insert/<esc> stop-insert")]
      (is (= {:insert {:esc {:call 'stop-insert}}} result))))
  (testing "Key with modifier"
    (let [result (bindings "#map/normal/<ctrl-f> stop-insert")]
      (is (= {:normal {:ctrl-f {:call 'stop-insert}}} result))))
  (testing "Multi-key mode map"
    (let [result (bindings "#map/normal/gi start-insert")]
      (is (= {:normal {:g {:i {:call 'start-insert}}}} result))))
  (testing "Multi-key mode map with a special key"
    (let [result (bindings "#map/normal/g<up> start-insert")]
      (is (= {:normal {:g {:up {:call 'start-insert}}}} result))))
  (testing "Settings"
    (let [result (settings "#set/timeout-len 500")]
      (is (= {:timeout-len 500} result)))
    (let [result (settings "#set/alias-file \"~/Foo.txt\"")]
      (is (= {:alias-file "~/Foo.txt"} result))))
  (testing "Aliases"
    (let [result (aliases "#alias \".afv Cleared as filed\"")]
      (is (= {".afv" {:alias ".afv"
                      :body "Cleared as filed"
                      :parts ["Cleared" "as" "filed"]}}
             result)))))
