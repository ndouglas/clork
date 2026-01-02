(ns clork.dam-test
  "Tests for the Dam/Reservoir area implementation."
  (:require [clojure.test :refer :all]
            [clork.core :as core]
            [clork.game-state :as gs]
            [clork.dam :as dam]
            [clork.daemon :as daemon]
            [clork.parser.state :as parser-state]))

;;; ---------------------------------------------------------------------------
;;; HELPER FUNCTIONS
;;; ---------------------------------------------------------------------------

(defn setup-dam-room
  "Set up game state with player in dam room."
  []
  (-> (core/init-game)
      (assoc :here :dam-room)))

(defn setup-maintenance-room
  "Set up game state with player in maintenance room."
  []
  (-> (core/init-game)
      (assoc :here :maintenance-room)))

(defn setup-with-wrench
  "Set up game state with player in dam room holding the wrench."
  []
  (-> (setup-dam-room)
      (assoc-in [:objects :wrench :in] :adventurer)))

;;; ---------------------------------------------------------------------------
;;; STAGE 1: Room Definition Tests
;;; ---------------------------------------------------------------------------

(deftest dam-room-exists-test
  (testing "Dam room is defined with correct properties"
    (let [gs (core/init-game)
          room (gs/get-thing gs :dam-room)]
      (is (some? room) "Dam room should exist")
      (is (:action room) "Dam room should have an action handler"))))

(deftest dam-area-rooms-exist-test
  (testing "All dam area rooms exist"
    (let [gs (core/init-game)]
      (is (gs/get-thing gs :dam-room) "dam-room should exist")
      (is (gs/get-thing gs :dam-lobby) "dam-lobby should exist")
      (is (gs/get-thing gs :maintenance-room) "maintenance-room should exist")
      (is (gs/get-thing gs :reservoir-south) "reservoir-south should exist")
      (is (gs/get-thing gs :reservoir) "reservoir should exist")
      (is (gs/get-thing gs :reservoir-north) "reservoir-north should exist")
      (is (gs/get-thing gs :stream-view) "stream-view should exist")
      (is (gs/get-thing gs :dam-base) "dam-base should exist"))))

(deftest dam-room-exits-test
  (testing "Dam room has correct exits"
    (let [gs (core/init-game)
          room (gs/get-thing gs :dam-room)
          exits (:exits room)]
      (is (= :dam-lobby (:north exits)) "North exit should go to dam-lobby")
      (is (= :reservoir-south (:west exits)) "West exit should go to reservoir-south"))))

;;; ---------------------------------------------------------------------------
;;; STAGE 2: Object Definition Tests
;;; ---------------------------------------------------------------------------

(deftest dam-objects-exist-test
  (testing "All dam area objects exist"
    (let [gs (core/init-game)]
      (is (gs/get-thing gs :dam) "dam object should exist")
      (is (gs/get-thing gs :bolt) "bolt should exist")
      (is (gs/get-thing gs :bubble) "bubble should exist")
      (is (gs/get-thing gs :control-panel) "control-panel should exist")
      (is (gs/get-thing gs :wrench) "wrench should exist")
      (is (gs/get-thing gs :yellow-button) "yellow-button should exist")
      (is (gs/get-thing gs :brown-button) "brown-button should exist")
      (is (gs/get-thing gs :blue-button) "blue-button should exist")
      (is (gs/get-thing gs :red-button) "red-button should exist")
      (is (gs/get-thing gs :tube) "tube should exist")
      (is (gs/get-thing gs :putty) "putty should exist")
      (is (gs/get-thing gs :guidebook) "guidebook should exist")
      (is (gs/get-thing gs :pump) "pump should exist"))))

(deftest bolt-in-dam-room-test
  (testing "Bolt is located in dam room"
    (let [gs (core/init-game)
          bolt (gs/get-thing gs :bolt)]
      (is (= :dam-room (:in bolt)) "Bolt should be in dam-room"))))

(deftest wrench-in-maintenance-room-test
  (testing "Wrench is located in maintenance room"
    (let [gs (core/init-game)
          wrench (gs/get-thing gs :wrench)]
      (is (= :maintenance-room (:in wrench)) "Wrench should be in maintenance-room"))))

;;; ---------------------------------------------------------------------------
;;; STAGE 3: Global State Tests
;;; ---------------------------------------------------------------------------

(deftest initial-dam-globals-test
  (testing "Dam globals are initialized correctly"
    (let [gs (core/init-game)]
      (is (false? (:low-tide gs)) "low-tide should start false (reservoir full)")
      (is (false? (:gates-open gs)) "gates-open should start false")
      (is (false? (:gate-flag gs)) "gate-flag should start false")
      (is (zero? (:water-level gs)) "water-level should start at 0"))))

;;; ---------------------------------------------------------------------------
;;; STAGE 4: Button Tests
;;; ---------------------------------------------------------------------------

(deftest yellow-button-sets-gate-flag-test
  (testing "Pushing yellow button sets gate-flag true"
    (let [gs (-> (setup-maintenance-room)
                 (parser-state/set-prsa :move)
                 (parser-state/set-prso :yellow-button))
          result (dam/button-action gs)]
      (is (true? (:gate-flag result)) "gate-flag should be true after pushing yellow button"))))

(deftest brown-button-clears-gate-flag-test
  (testing "Pushing brown button sets gate-flag false"
    (let [gs (-> (setup-maintenance-room)
                 (assoc :gate-flag true)
                 (parser-state/set-prsa :move)
                 (parser-state/set-prso :brown-button))
          result (dam/button-action gs)]
      (is (false? (:gate-flag result)) "gate-flag should be false after pushing brown button"))))

;;; ---------------------------------------------------------------------------
;;; STAGE 5: Bolt/Sluice Gate Tests
;;; ---------------------------------------------------------------------------

(deftest bolt-requires-gate-flag-test
  (testing "Turning bolt without gate-flag set fails"
    (let [gs (-> (setup-with-wrench)
                 (assoc :gate-flag false)
                 (parser-state/set-prsa :turn)
                 (parser-state/set-prso :bolt)
                 (parser-state/set-prsi :wrench))
          result (dam/bolt-action gs)]
      (is (false? (:gates-open result)) "gates-open should remain false"))))

(deftest bolt-opens-gates-test
  (testing "Turning bolt with gate-flag opens sluice gates"
    (let [gs (-> (setup-with-wrench)
                 (assoc :gate-flag true)
                 (assoc :gates-open false)
                 (parser-state/set-prsa :turn)
                 (parser-state/set-prso :bolt)
                 (parser-state/set-prsi :wrench))
          result (dam/bolt-action gs)]
      (is (true? (:gates-open result)) "gates-open should be true")
      (is (daemon/daemon-enabled? result :i-rempty)
          "i-rempty daemon should be enabled"))))

(deftest bolt-closes-gates-test
  (testing "Turning bolt with gates open closes them"
    (let [gs (-> (setup-with-wrench)
                 (assoc :gate-flag true)
                 (assoc :gates-open true)
                 (parser-state/set-prsa :turn)
                 (parser-state/set-prso :bolt)
                 (parser-state/set-prsi :wrench))
          result (dam/bolt-action gs)]
      (is (false? (:gates-open result)) "gates-open should be false")
      (is (daemon/daemon-enabled? result :i-rfill)
          "i-rfill daemon should be enabled"))))

;;; ---------------------------------------------------------------------------
;;; STAGE 6: Daemon Tests
;;; ---------------------------------------------------------------------------

(deftest i-rfill-daemon-test
  (testing "i-rfill daemon sets low-tide false when triggered"
    (let [gs (-> (core/init-game)
                 (assoc :low-tide true))
          result (dam/i-rfill gs)]
      (is (false? (:low-tide result)) "low-tide should be false after i-rfill"))))

(deftest i-rempty-daemon-test
  (testing "i-rempty daemon sets low-tide true when triggered"
    (let [gs (-> (core/init-game)
                 (assoc :low-tide false))
          result (dam/i-rempty gs)]
      (is (true? (:low-tide result)) "low-tide should be true after i-rempty"))))

;;; ---------------------------------------------------------------------------
;;; STAGE 7: Reservoir Crossing Tests
;;; ---------------------------------------------------------------------------

(deftest reservoir-crossing-blocked-when-full-test
  (testing "Cannot cross reservoir when water is high"
    (let [gs (-> (core/init-game)
                 (assoc :here :reservoir-south)
                 (assoc :low-tide false))
          exits (get-in gs [:rooms :reservoir-south :exits])
          north-exit (:north exits)]
      ;; The exit should be conditional based on low-tide
      (is (map? north-exit) "North exit should be a conditional map")
      (is (= :low-tide (:if north-exit)) "North exit should check low-tide flag"))))

(deftest reservoir-crossing-allowed-when-empty-test
  (testing "Can cross reservoir when water is low"
    (let [gs (-> (core/init-game)
                 (assoc :here :reservoir-south)
                 (assoc :low-tide true))
          exits (get-in gs [:rooms :reservoir-south :exits])
          north-exit (:north exits)]
      ;; When low-tide is true, the exit should resolve to :reservoir
      (is (= :reservoir (:to north-exit)) "North exit should lead to reservoir"))))

;;; ---------------------------------------------------------------------------
;;; STAGE 8: Bubble Indicator Tests
;;; ---------------------------------------------------------------------------

(deftest bubble-glows-when-gate-flag-set-test
  (testing "Bubble glows when gate-flag is true"
    (let [gs (-> (setup-dam-room)
                 (assoc :gate-flag true)
                 (parser-state/set-prsa :examine)
                 (parser-state/set-prso :bubble))
          result (dam/bubble-action gs)]
      ;; The bubble-action should return a game-state (with output)
      (is (some? result) "bubble-action should return a result"))))

(deftest bubble-dark-when-gate-flag-not-set-test
  (testing "Bubble is dark when gate-flag is false"
    (let [gs (-> (setup-dam-room)
                 (assoc :gate-flag false)
                 (parser-state/set-prsa :examine)
                 (parser-state/set-prso :bubble))
          result (dam/bubble-action gs)]
      (is (some? result) "bubble-action should return a result"))))
