(ns xradar.sector-scene-test
  (:require [clojure
             [test :refer :all]
             [string :refer [join]]]
            [xradar
             [scene :refer :all]
             [sector-scene :refer :all]
             [util :refer [coord-scale]]])
  (:import [java.io StringReader]))

;;
;; Consts
;;

(def data-info
  "[INFO]\nBoston Tower v5.0_FC\nBOS_TWR\nKBOS\nN042.20.54.750\nW071.00.21.920\n60\n45\n16\n1")

(def data-airport
  "[AIRPORT]\nKBOS 128.800 N042.00.00.000 W070.00.00.000 B")

(def data-geo
  (str "#define Taxiway 14737632\n"
       "[GEO]\n"
       "N042.00.00.000 W071.00.00.000 N042.00.00.000 W071.00.00.000 Taxiway\n"
       "N042.00.00.000 W071.00.00.000 N042.00.00.000 W071.00.00.000 255" ))

(def data-shape
  (str "#define Taxiway 14737632\n"
       "[GEO]\n"
       "N042.00.00.000 W071.00.00.000 N043.00.00.000 W072.00.00.000 Taxiway\n"
       "N043.00.00.000 W072.00.00.000 N044.00.00.000 W072.00.00.000 Taxiway" ))


(def data-labels
  (str "#define Taxiway 14737632\n"
       "[LABELS]\n"
       "\"S\" N042.00.00.000 W071.00.00.000 Taxiway\n"
       "\"Awesome Spot\" N042.00.00.000 W071.00.00.000 255"))

(def data-runways
  (str
    "[RUNWAY]\n" 
    "04L 22R 000 000 N042.00.00.000 W071.00.00.000 N043.00.00.000 W072.00.00.000\n"
    "04R 22L 036 216 N044.00.00.000 W073.00.00.000 N045.00.00.000 W074.00.00.000"))

(def sid-name "+ Video Map - LGA")
(def data-sid
  (str "#define Taxiway 14737632\n"
       "[SID]\n"
       sid-name "               "
       "N000.00.000 W000.00.00.000 N000.00.00.000 W000.00.00.000\n"
       "                                "
       "N042.00.00.000 W071.00.00.000 N043.00.00.000 W072.00.00.000 Taxiway\n"
       "                                "
       "N043.00.00.000 W072.00.00.000 N044.00.00.000 W072.00.00.000 Taxiway"))

(def data-vor
  "[VOR]\nBOS 112.700 N042.00.00.000 W070.00.00.000")

(def scene (->SectorScene (atom {})))

;;
;; Utils
;;

(defn load-data
  [string]
  (load-sector-data (StringReader. string)))

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
    (is (== -152478037/3600000 
            (parse-coord "N042.21.18.037")))
    (is (== 42.355 
            (parse-coord "S042.21.18.000")))
    (is (= {:x 42 :y 42} 
           (parse-coord scene 
                        "S042.00.00.000"
                        "E042.00.00.000"))))
  (testing "parse-coord with waypoints"
    (let [scene (load-data data-vor)]
      ;; be lazy and hope we mapped correctly
      (is (not= {:x 42 :y 70} 
                (parse-coord scene "BOS" "BOS")))))
  (testing "parse-coord with scale"
    (let [scene (load-data data-info)]
      ;; be lazy and hope we mapped correctly
      (is (not= {:x 42 :y 42} 
                (parse-coord scene 
                             "S042.00.00.000"
                             "E042.00.00.000"))))))

(deftest load-sector-test
  (testing "#define"
    (let [data (load-data "#define Foo 255")]
      (is (= 0xffFF0000 (-> data :colors :Foo)))))
  (testing "[INFO]"
    (let [data (load-data data-info)]
      (is (= "Boston Tower v5.0_FC" (-> data :info :name)))
      (is (= 45 (-> data :info :nm-per-lon)))
      (is (== 16 (-> data :info :magnet-var)))))
  (testing "[VOR]"
    (let [data (load-data data-vor)]
      (is (= [{:name "BOS"
               :freq "112.700"
               :coord {:x (* -70 coord-scale)
                       :y (* -42 coord-scale)}}] (-> data :vor)))))
  (testing "[AIRPORT]"
    (let [data 
          (load-data data-airport)]
      (is (= [{:name "KBOS"
               :freq "128.800"
               :coord {:x (* -70 coord-scale) :y (* -42 coord-scale)}
               :airspace "B"}] (-> data :airport)))))
  (testing "[GEO]"
    (let [data (load-data data-geo)]
      (is (= [{:start {:x (* -71 coord-scale) :y (* -42 coord-scale)}
               :end {:x (* -71 coord-scale) :y (* -42 coord-scale)}
               :color 0xffE0E0E0}
              {:start {:x (* -71 coord-scale) :y (* -42 coord-scale)}
               :end {:x (* -71 coord-scale) :y (* -42 coord-scale)}
               :color 0xffFF0000}]
             (-> data :geo)))))
  (testing "[LABELS]"
    (let [data (load-data data-labels)]
      (is (= [{:label "S"
               :coord {:x (* -71 coord-scale) :y (* -42 coord-scale)}
               :color 0xffE0E0E0}
              {:label "Awesome Spot"
               :coord {:x (* -71 coord-scale) :y (* -42 coord-scale)}
               :color 0xffFF0000}]
             (-> data :labels)))))
  (testing "[RUNWAY]"
    (let [data (load-data data-runways)]
      (is (= [{:labels ["04L" "22R"]
               :magnetic [0 0]
               :start {:x (* -71 coord-scale) :y (* -42 coord-scale)}
               :end {:x (* -72 coord-scale) :y (* -43 coord-scale)}}
              {:labels ["04R" "22L"]
               :magnetic [36 216]
               :start {:x (* -73 coord-scale) :y (* -44 coord-scale)}
               :end {:x (* -74 coord-scale) :y (* -45 coord-scale)}}]
             (-> data :runway))))))

(deftest shapes-test
  (testing "Single shape shapes"
    (let [data (load-data data-geo)]
      (is (= [[{:x (* -71 coord-scale) :y (* -42 coord-scale)}
               {:x (* -71 coord-scale) :y (* -42 coord-scale)}]
              [{:x (* -71 coord-scale) :y (* -42 coord-scale)}
               {:x (* -71 coord-scale) :y (* -42 coord-scale)}]]
             (-> data :geo-shapes)))
      (is (= 0xffE0E0E0
             (-> data :geo-shapes first meta :color)))
      (is (= 0xffFF0000
             (-> data :geo-shapes second meta :color)))))
  (testing "Multi-item shapes"
    (let [data (load-data data-shape)]
      (is (= [[{:x (* -71 coord-scale) :y (* -42 coord-scale)}
               {:x (* -72 coord-scale) :y (* -43 coord-scale)}
               {:x (* -72 coord-scale) :y (* -44 coord-scale)}]]
             (-> data :geo-shapes)))
      (is (= {:color 0xffE0E0E0
              :name nil
              :bounds [(* -72 coord-scale)   ; left (min-x)
                       (* -44 coord-scale)   ; top (min-y)
                       (* -71 coord-scale)   ; right (max-x)
                       (* -42 coord-scale)]} ; bottom (max-y)
             (-> data :geo-shapes first meta)))))
  (testing "SIDs (and STARs)"
    (let [data (load-data data-sid)]
      (is (not (nil? (parse-lazy-diagram-lines 
                       data
                       :sid (get-in data [:sid sid-name]))))))
    (let [data (load-data data-sid)]
      (clear-inflated-cache)
      (ensure-diagram-inflated :sid data sid-name)
      (is (= [[{:x (* -71 coord-scale) :y (* -42 coord-scale)}
               {:x (* -72 coord-scale) :y (* -43 coord-scale)}
               {:x (* -72 coord-scale) :y (* -44 coord-scale)}
               ]]
             (-> @lazy-inflated-cache :sid-shapes)))
      ;; FIXME restore
      #_(is (= {:color 0xffE0E0E0
              :name "+ Video Map - LGA"
              :bounds [(* -72 coord-scale)   ; left (min-x)
                       (* -44 coord-scale)   ; top (min-y)
                       (* -71 coord-scale)   ; right (max-x)
                       (* -42 coord-scale)]} ; bottom (max-y)
             (-> @lazy-inflated-cache :sid-shapes first meta)))
      #_(clear-inflated-cache))))

(deftest methods-test
  (testing "find-point"
    (let [scene (load-sector-sync 
                  (StringReader. 
                    (join "\n" [data-info data-airport data-vor])))]
      (let [kbos (find-point scene "KBOS")]
        (is (= "KBOS" (:name kbos)))
        (is (= "128.800" (:freq kbos))))
      (let [kbos (find-point scene "kbos")]
        (is (= "KBOS" (:name kbos)))
        (is (= "128.800" (:freq kbos))))
      (let [bos (find-point scene "BOS")]
        (is (= "BOS" (:name bos)))
        (is (= "112.700" (:freq bos)))))))
