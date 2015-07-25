(ns xradar.sector-scene-test
  (:require [clojure.test :refer :all]
            [xradar.sector-scene :refer :all]))

;;
;; Consts
;;

(def data-info
  "[INFO]\nBoston Tower v5.0_FC\nBOS_TWR\nKBOS\nN042.20.54.750\nW071.00.21.920\n60\n45\n16\n1")

(def data-geo
  (str "#define Taxiway 14737632\n"
       "[GEO]\n"
       "N042.00.00.000 W071.00.00.000 N042.00.00.000 W071.00.00.000 Taxiway\n"
       "N042.00.00.000 W071.00.00.000 N042.00.00.000 W071.00.00.000 255" ))

(def data-labels
  (str "#define Taxiway 14737632\n"
       "[LABELS]\n"
       "\"S\" N042.00.00.000 W071.00.00.000 Taxiway\n"
       "\"Awesome Spot\" N042.00.00.000 W071.00.00.000 255"))

;;
;; Utils
;;

(defn load-data
  [string]
  (load-sector-data (java.io.StringReader. string)))

;; 
;; Tests
;;

(deftest utils-tests
  (testing "parse-color"
    (is (= 0xffFF0000 (parse-color "255")))
    (is (= 0xff00FF00 (parse-color "65280")))
    (is (= 0xff0000FF (parse-color "16711680")))
    ;; colors from ZNY
    (is (= 0xff606060 (parse-color "6316128")))
    (is (= 0xff142A00 (parse-color "10772"))))
  (testing "parse-coord"
    (is (== 152478037/3600000 (parse-coord "N042.21.18.037")))
    (is (== -42.355 (parse-coord "S042.21.18.000")))
    (is (= {:x 42 :y -42} 
           (parse-coord "S042.00.00.000"
                        "E042.00.00.000")))))

(deftest load-sector-test
  (testing "#define"
    (let [data (load-data "#define Foo 255")]
      (is (= 0xffFF0000 (-> data :colors :Foo)))))
  (testing "[INFO]"
    (let [data (load-data data-info)]
      (is (= "Boston Tower v5.0_FC" (-> data :info :name)))
      (is (= 45 (-> data :info :nm-per-lon)))
      (is (= 16 (-> data :info :magnet-var)))))
  (testing "[VOR]"
    (let [data (load-data "[VOR]\nBOS 112.700 N042.00.00.000 W070.00.00.000")]
      (is (= [{:name "BOS"
               :freq "112.700"
               :coord {:x -70 :y 42}}] (-> data :vor)))))
  (testing "[AIRPORT]"
    (let [data 
          (load-data "[AIRPORT]\nKBOS 128.800 N042.00.00.000 W070.00.00.000 B")]
      (is (= [{:name "KBOS"
               :freq "128.800"
               :coord {:x -70 :y 42}
               :airspace "B"}] (-> data :airport)))))
  (testing "[GEO]"
    (let [data (load-data data-geo)]
      (is (= [{:start {:x -71 :y 42}
               :end {:x -71 :y 42}
               :color 0xffE0E0E0}
              {:start {:x -71 :y 42}
               :end {:x -71 :y 42}
               :color 0xffFF0000}]
             (-> data :geo)))))
  (testing "[LABELS]"
    (let [data (load-data data-labels)]
      (is (= [{:label "S"
               :coord {:x -71 :y 42}
               :color 0xffE0E0E0}
              {:label "Awesome Spot"
               :coord {:x -71 :y 42}
               :color 0xffFF0000}]
             (-> data :labels))))))
