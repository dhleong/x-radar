(ns xradar.notif-test
  (:require [clojure.test :refer :all]
            [xradar
             [notif :refer :all]
             [util :refer [in-focus?]]]))

(defmacro request-without-focus!
  [& body]
  `(with-redefs-fn {#'in-focus? (constantly false)}
    #(request-attention! ~@body)))

(defmacro request-with-focus!
  [& body]
  `(with-redefs-fn {#'in-focus? (constantly true)}
    #(request-attention! ~@body)))

(deftest request-attention-test
  (testing "Request topic with focus does nothing"
    (is (empty? (requested-topics)))
    (request-with-focus! :topic :fp)
    (is (empty? (requested-topics))))
  (testing "Request topic without focus"
    (is (empty? (requested-topics)))
    (request-without-focus! :topic :fp)
    (is (= #{:fp} (requested-topics)))
    (ack-attention!))
  (testing "Critical topic with focus"
    (is (empty? (requested-topics)))
    (request-with-focus! :topic :fp :is-critical true)
    (is (= #{:fp} (requested-topics)))
    ;; simple ack does nothing
    (ack-attention!)
    (is (= #{:fp} (requested-topics)))
    ;; must explicitly ack ciritical topics
    (ack-attention! :fp)
    (is (empty? (requested-topics))))
  (testing "Critical topic without focus"
    (is (empty? (requested-topics)))
    (request-without-focus! :topic :fp :is-critical true)
    (is (= #{:fp} (requested-topics)))
    ;; simple ack still does nothing
    (ack-attention!)
    (is (= #{:fp} (requested-topics)))
    ;; must explicitly ack ciritical topics
    (ack-attention! :fp)
    (is (empty? (requested-topics)))))
