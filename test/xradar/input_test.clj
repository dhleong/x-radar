(ns xradar.input-test
  (:refer-clojure :exclude [empty])
  (:require [clojure.test :refer :all]
            [xradar
             [input :refer :all]
             [voice :refer :all]]))

(deftype DummyVoice [state-atom]
  XVoice
  (connect!
    [this config]
    (swap! state-atom assoc :connected? true))
  (connected?
    [this]
    (:connected? @state-atom))
  (start-transmitting
    [this]
    (swap! state-atom #(assoc % :transmitting?
                              (inc (:transmitting? %)))))
  (stop-transmitting
    [this]
    (swap! state-atom #(assoc % :transmitting?
                              (dec (:transmitting? %)))))
  (transmitting?
    [this]
    (> (:transmitting? @state-atom) 0))
  (transmitting?
    [this connection-name]
    (> (:transmitting? @state-atom) 0)))
(defn new-voice []
  (->DummyVoice (atom {:transmitting? 0})))
(defn transmitting-count [voice]
  (:transmitting? @(.-state-atom voice)))

(defn empty [] @(create-input {}))
(defn- with-mods [& mods]
  (let [base (create-input {})]
    (doseq [m mods]
      (process-input-press base {} {:key m}))
    @base))
(defn shifted [] (with-mods :shift))

(deftest translate-event-test
  (testing "Special keys"
    (let [original {:key :esc :key-code 27}
          translated (translate-event (empty) original)]
      (is (= original translated)))
    (let [original {:key-code 59 :raw-key \: :key (keyword ";")}
          translated (translate-event (empty) original)]
      (is (= {:key :colon :key-code 59 :raw-key \:}
             translated))))
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
  (testing "Handle scroll"
    (let [original (with-mods :control) 
          state (atom {:current-output :global
                       :output-buffer (atom [{:text "1"} {:text "2"}
                                             {:text "3"}])
                       :profile {:output-size 1}
                       :output-metrics-cache {:chars-per-line 100}
                       :output-scroll 0})
          machine (process-press original 
                                 state 
                                 {:key :b :key-code 66})]
      (is (= 1 (:output-scroll @state)))
      ;; we should be where we expect to be
      (is (contains? (:current-bindings machine) :ctrl-b))))
  (testing "Multi-key map"
    (let [original (empty)
          state (atom {})
          machine (process-press original state {:key :o :key-code 79})]
      (is (contains? (:current-bindings machine) :f)))))

(def space-event
  {:key :space :key-code 32})

(deftest transmit-voice-test
  (testing "Just echo when not connected"
    (let [voice (new-voice)
          machine (empty)
          state (atom {:voice voice})
          after-press
          (process-press machine state space-event) ]
      ;; not connected? just warn and do nothing
      (is (= "No primary voice channel connected"
             (first (:last-echo after-press))))
      (is (= 0 (transmitting-count voice)))
      (process-release machine state space-event)
      (is (= 0 (transmitting-count voice)))))
  (testing "No dup starts" 
    (let [voice (new-voice)
          machine (empty)
          state (atom {:voice voice})]
      (connect! voice machine)
      (is (= 0 (transmitting-count voice)))
      (process-press machine state space-event)
      (is (= 1 (transmitting-count voice)))
      (process-press machine state space-event)
      (is (= 1 (transmitting-count voice)))))
  (testing "Release to stop" 
    (let [voice (new-voice)
          machine (empty)
          state (atom {:voice voice})]
      (connect! voice machine)
      (is (= 0 (transmitting-count voice)))
      (process-press machine state space-event)
      (is (= 1 (transmitting-count voice)))
      (process-release machine state space-event)
      (is (= 0 (transmitting-count voice))))))
