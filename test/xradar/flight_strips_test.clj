(ns xradar.flight-strips-test
  (:require [clojure.test :refer :all]
            [xradar.flight-strips :refer :all]))

(def fields {:arrive ["KLGA"]
             :depart ["KLGA"]})

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

(defn del
  [bay & col]
  (delete-current-strip bay)
  (if col
    (get-col bay col)
    (get-col bay 0)))


(deftest resolve-flight-type-test
  (testing "Arrivals"
    (is (= :arrival (resolve-flight-type
                      fields 
                      {:rules :ifr 
                       :route "Any"
                       :depart "KBOS" 
                       :arrive "KLGA"})))
    (is (= :arrival (resolve-flight-type
                      fields 
                      {:rules :vfr
                       :route "Any"
                       :depart "KBOS" 
                       :arrive "KLGA"}))))
  (testing "Departures"
    (is (= :departure (resolve-flight-type
                        fields 
                        {:rules :ifr
                         :route "Any"
                         :depart "KLGA"
                         :arrive "KBOS"}))))
  (testing "VFR"
    (is (= :vfr (resolve-flight-type
                  fields 
                  {:rules :vfr
                   :depart "KLGA"
                   :arrive "KBOS"}))))
  (testing "Local flights"
    (is (= :local (resolve-flight-type
                    fields 
                    {:rules :vfr
                     :depart "KLGA"
                     :arrive "KLGA"})))
    (is (= :local (resolve-flight-type
                    fields 
                    {:rules :ifr
                     :depart "KLGA"
                     :arrive "KLGA"}))))
  (testing "Overflights"
    (is (= :over (resolve-flight-type
                   fields 
                   {:rules :vfr
                    :route "Any"
                    :depart "KJFK"
                    :arrive "KBOS"})))
    (is (= :over (resolve-flight-type
                   fields 
                   {:rules :ifr
                    :route "Any"
                    :depart "KJFK"
                    :arrive "KBOS"}))))
  (testing "Unknown"
    (is (= :unknown (resolve-flight-type
                      {:arrive [] :depart []}
                      {:rules :vfr
                       :route "Any"
                       :depart "KLGA"
                       :arrive "KBOS"})))
    (is (= :unknown (resolve-flight-type
                      {:arrive [] :depart []}
                      {:rules :ifr
                       :route "Any"
                       :depart "KLGA"
                       :arrive "KBOS"}))))
  (testing "No plan loaded"
    (is (= :noplan (resolve-flight-type
                     fields 
                     {:rules :ifr
                      :depart "KLGA"
                      :arrive "KBOS"})))))

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

(deftest delete-strip-test
  (testing "Normal"
    (let [bay (create-strip-bay)]
      (swap! bay assoc 0 ["Foo" "Bar"])
      (is (= ["Foo" "Bar"] (get-col bay 0)))
      (is (= ["Bar"] (del bay)))
      (is (= [] (del bay))))))
