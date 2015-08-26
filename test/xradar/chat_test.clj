(ns xradar.chat-test
  (:require [clojure.test :refer :all]
            [xradar.chat :refer :all]
            [xradar.output :refer [create-output-buffers set-active!]]
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
      (is (= "Test" (:text (last-output state))))
      (is (= "Test" (last-sent state)))))
  (testing "Connected, selection"
    (let [state (new-radar)]
      (do-connect! state)
      (swap! state 
             assoc 
             :aircraft {42 {:callsign "ACA42"}}
             :selected 42)
      (send-chat! state "Test")
      (is (= "ACA42, Test" (:text (last-output state))))
      (is (nil? (:with (last-output state))))
      (is (= "ACA42, Test" (last-sent state)))))
  (testing "Connected, filtered, no selection"
    (let [state (new-radar)]
      (do-connect! state)
      (set-active! state 42)
      (send-chat! state "Test")
      (is (= "Test" (:text (last-output state))))
      (is (= 42 (:with (last-output state))))
      (is (= "Test" (:message (last-sent state))))
      (is (= 42 (:cid (last-sent state))))))
  (testing "Connected, filtered, with selection (should be same as w/o)"
    (let [state (new-radar)]
      (do-connect! state)
      (swap! state 
             assoc 
             :aircraft {9001 {:callsign "ACA9001"}}
             :selected 9001)
      (set-active! state 42)
      (send-chat! state "Test")
      (is (= "Test" (:text (last-output state))))
      (is (= 42 (:with (last-output state))))
      (is (= "Test" (:message (last-sent state))))
      (is (= 42 (:cid (last-sent state)))))))
