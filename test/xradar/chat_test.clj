(ns xradar.chat-test
  (:require [clojure.test :refer :all]
            [xradar.chat :refer :all]
            [xradar.output :refer [create-output-buffers
                                  get-active-buffer set-active!]]
            [xradar.network :refer :all]))

(defn- new-radar
  []
  (let [network-output (atom [])
        is-connected? (atom false)
        controllers (atom [])]
    (atom (assoc
           (create-output-buffers)
           :controllers controllers
           :network-output network-output
           :network 
           (reify XRadarNetwork
             (connected? [this]
               @is-connected?)
             (connect! [this params]
               (swap! is-connected? (constantly true)))
             (get-controllers [this]
               @controllers)
             (my-callsign [this]
               "LGA_GND")
             (send! [this message]
               (swap! network-output conj message))
             (send-to! [this cid message]
               (swap! network-output conj {:cid cid :message message})))))))

(defn- last-sent
  [state]
  (first @(:network-output @state)))

(defn- last-output
  [state]
  (first @(:output-buffer @state)))

(defn- last-shown
  [state]
  (first (get-active-buffer @state)))


(defn- do-connect!
  [state]
  (connect! (:network @state) {}))

(deftest send-chat-test
  (testing "Not connected"
    (let [state (new-radar)]
      (send-chat! state "Test")
      (is (= "ERR: Not Connected" (:text (last-output state))))
      ;; no network, so nothing should get sent
      (is (nil? (last-sent state)))))
  (testing "Connected, no selection"
    (let [state (new-radar)]
      (do-connect! state)
      (send-chat! state "Test")
      (is (= "LGA_GND: Test" (:text (last-output state))))
      (is (= "Test" (last-sent state)))))
  (testing "Connected, selection"
    (let [state (new-radar)]
      (do-connect! state)
      (swap! state 
             assoc 
             :aircraft {42 {:callsign "ACA42"}}
             :selected 42)
      (send-chat! state "Test")
      (is (= "LGA_GND: ACA42, Test" (:text (last-output state))))
      (is (nil? (:with (last-output state))))
      (is (= "ACA42, Test" (last-sent state)))))
  (testing "Connected, filtered, no selection"
    (let [state (new-radar)]
      (do-connect! state)
      (swap! state 
             assoc 
             :aircraft {42 {:callsign "ACA42"}}
             :selected 42)
      (set-active! state 42)
      (send-chat! state "Test")
      (is (= "LGA_GND: Test" (:text (last-output state))))
      (is (= 42 (:with (last-output state))))
      (is (= "Test" (:message (last-sent state))))
      (is (= 42 (:cid (last-sent state))))))
  (testing "Connected, filtered, with selection (should be same as w/o)"
    (let [state (new-radar)]
      (do-connect! state)
      (swap! state 
             assoc 
             :aircraft {9001 {:callsign "ACA9001"}
                        42 {:callsign "ACA42"}}
             :selected 9001)
      (set-active! state 42)
      (send-chat! state "Test")
      (is (= "LGA_GND: Test" (:text (last-output state))))
      (is (= 42 (:with (last-output state))))
      (is (= "Test" (:message (last-sent state))))
      (is (= 42 (:cid (last-sent state)))))))

(deftest receive-private-test
  (testing "Receive private in global"
    (let [state (new-radar)]
      (do-connect! state)
      (swap! state 
             assoc 
             :aircraft {9001 {:callsign "ACA9001"}})
      (receive-from-private state 9001 "Test")
      (is (= "ACA9001: Test" (:text (last-output state))))
      (is (= 9001 (:with (last-output state))))
      (is (= "<ACA9001> ACA9001: Test" (:text (last-shown state))))))
  (testing "Receive private when filtered"
    (let [state (new-radar)]
      (do-connect! state)
      (swap! state 
             assoc 
             :aircraft {9001 {:callsign "ACA9001"}})
      (set-active! state 9001)
      (receive-from-private state 9001 "Test")
      (is (= "ACA9001: Test" (:text (last-output state))))
      (is (= 9001 (:with (last-output state))))
      (is (= "ACA9001: Test" (:text (last-shown state)))))))

(deftest receive-public-test
  (testing "Receive global chat"
    (let [state (new-radar)]
      (do-connect! state)
      (swap! state 
             assoc 
             :aircraft {9001 {:callsign "ACA9001"}
                        42 {:callsign "ACA42"}})
      (receive-from state 42 "Test")
      (is (= "ACA42: Test" (:text (last-output state))))
      (is (nil? (:with (last-output state))))
      (is (= "ACA42: Test" (:text (last-shown state))))))
  (testing "Receive global when filtered"
    (let [state (new-radar)]
      (do-connect! state)
      (swap! state 
             assoc 
             :aircraft {9001 {:callsign "ACA9001"}
                        42 {:callsign "ACA42"}})
      (set-active! state 9001)
      (receive-from state 42 "Test")
      (is (= "ACA42: Test" (:text (last-output state))))
      (is (nil? (:with (last-output state))))
      (is (nil? (last-shown state))))))
