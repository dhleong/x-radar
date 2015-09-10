(ns xradar.lists-test
  (:require [clojure.test :refer :all]
            [xradar.lists :refer :all]))

(deftest sources-test
  (testing "Arrivals"
    (is (= [{:cid 4 :arrive "KLGA"}]
           (items-for 
             {:aircraft 
              {4 {:cid 4 :arrive "KLGA"}}
              :profile
              {:arrivals #{"KLGA"}}}
             :arrivals)))
    (is (empty?
          (items-for 
            {:aircraft 
             {4 {:cid 4 :arrive "KLGA"}}
             :profile
             {:arrivals #{"KJFK"}}}
            :arrivals))))
  (testing "Departures"
    (is (= [{:cid 4 :depart "KLGA"}]
           (items-for 
             {:aircraft 
              {4 {:cid 4 :depart "KLGA"}}
              :profile
              {:departures #{"KLGA"}}}
             :departures)))
    (is (empty?
          (items-for 
            {:aircraft 
             {4 {:cid 4 :depart "KLGA"}}
             :profile
             {:departures #{"KJFK"}}}
            :departures)))))

(deftest render-value-test
  (testing "Render String"
    (is (= "The String" 
           (render-value nil 
                         "The String" 
                         {:foo "bar"
                          :biz "baz"}))))
  (testing "Render Keyword"
    (is (= "bar" 
           (render-value nil 
                         :foo 
                         {:foo "bar"
                          :biz "baz"}))))
  (testing "Render Function"
    (is (= "baz" 
           (render-value nil 
                         #(:biz %2)
                         {:foo "bar"
                          :biz "baz"})))))

(deftest render-entry-test
  (testing "Departure"
    (let [aircraft {:cid 4 :squawk 1102
                    :depart "KLGA"
                    :arrive "KBOS"
                    :callsign "ACA1234"}
          radar {:aircraft {4 aircraft}}]
      (is (= ["ACA1234 "
              "D"
              "KLGA"
              "1102"]
             (render-list-entry
               radar
               (:departures list-metas)
               aircraft))))))
