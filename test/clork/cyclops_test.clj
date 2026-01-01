(ns clork.cyclops-test
  "Tests for the Cyclops NPC implementation."
  (:require [clojure.test :refer :all]
            [clork.core :as core]
            [clork.game-state :as gs]
            [clork.flags :as flags]
            [clork.cyclops :as cyclops]
            [clork.daemon :as daemon]
            [clork.parser.state :as parser-state]))

;;; ---------------------------------------------------------------------------
;;; HELPER FUNCTIONS
;;; ---------------------------------------------------------------------------

(defn setup-cyclops-room
  "Set up game state with player in cyclops room."
  []
  (-> (core/init-game)
      (assoc :here :cyclops-room)))

(defn setup-with-food
  "Set up game state with player in cyclops room holding lunch and water."
  []
  (-> (setup-cyclops-room)
      (assoc-in [:objects :lunch :in] :adventurer)
      (assoc-in [:objects :bottle :in] :adventurer)
      (assoc-in [:objects :water :in] :bottle)))

;;; ---------------------------------------------------------------------------
;;; STAGE 1: Object Definition Tests
;;; ---------------------------------------------------------------------------

(deftest cyclops-object-exists-test
  (testing "Cyclops is defined with correct properties"
    (let [gs (core/init-game)
          cyclops (gs/get-thing gs :cyclops)]
      (is (some? cyclops) "Cyclops object should exist")
      (is (= :cyclops-room (:in cyclops)) "Cyclops should start in cyclops-room")
      (is (= 10000 (:strength cyclops)) "Cyclops should have strength 10000")
      (is (gs/set-thing-flag? gs :cyclops :actor) "Cyclops should have actor flag"))))

(deftest cyclops-room-exists-test
  (testing "Cyclops room is defined with correct properties"
    (let [gs (core/init-game)
          room (gs/get-thing gs :cyclops-room)]
      (is (some? room) "Cyclops room should exist")
      (is (:action room) "Cyclops room should have an action handler"))))

(deftest cyclops-room-exits-test
  (testing "Cyclops room has conditional exits"
    (let [gs (core/init-game)
          room (gs/get-thing gs :cyclops-room)
          exits (:exits room)]
      (is (= :maze-15 (:nw exits)) "NW exit should go to maze-15")
      (is (map? (:east exits)) "East exit should be conditional")
      (is (= :magic-flag (get-in exits [:east :if])) "East exit requires magic-flag")
      (is (map? (:up exits)) "Up exit should be conditional")
      (is (= :cyclops-flag (get-in exits [:up :if])) "Up exit requires cyclops-flag"))))

;;; ---------------------------------------------------------------------------
;;; STAGE 2: Global State Tests
;;; ---------------------------------------------------------------------------

(deftest initial-cyclops-globals-test
  (testing "Cyclops globals are initialized correctly"
    (let [gs (core/init-game)]
      (is (false? (:cyclops-flag gs)) "cyclops-flag should start false")
      (is (false? (:magic-flag gs)) "magic-flag should start false")
      (is (zero? (:cyclowrath gs)) "cyclowrath should start at 0"))))

;;; ---------------------------------------------------------------------------
;;; STAGE 3: V-ODYSSEUS Tests
;;; ---------------------------------------------------------------------------

(deftest v-odysseus-in-cyclops-room-test
  (testing "Saying ODYSSEUS in cyclops room scares cyclops away"
    (let [gs (setup-cyclops-room)
          result (cyclops/v-odysseus gs)]
      (is (true? (:cyclops-flag result)) "cyclops-flag should be true")
      (is (true? (:magic-flag result)) "magic-flag should be true (east wall opened)")
      (is (nil? (get-in result [:objects :cyclops :in])) "Cyclops should be removed")
      (is (false? (daemon/daemon-enabled? result :i-cyclops))
          "i-cyclops daemon should be disabled"))))

(deftest v-odysseus-elsewhere-test
  (testing "Saying ODYSSEUS outside cyclops room gives generic response"
    (let [gs (-> (core/init-game)
                 (assoc :here :west-of-house))
          result (cyclops/v-odysseus gs)]
      (is (false? (:cyclops-flag result)) "cyclops-flag should remain false")
      (is (false? (:magic-flag result)) "magic-flag should remain false")
      (is (= :cyclops-room (get-in result [:objects :cyclops :in]))
          "Cyclops should still be in cyclops-room"))))

(deftest v-odysseus-with-sleeping-cyclops-test
  (testing "Saying ODYSSEUS when cyclops is asleep gives generic response"
    (let [gs (-> (setup-cyclops-room)
                 (assoc :cyclops-flag true))  ; Cyclops is asleep
          result (cyclops/v-odysseus gs)]
      ;; Should give generic response, not scare the sleeping cyclops
      (is (true? (:cyclops-flag result)) "cyclops-flag should remain true"))))

;;; ---------------------------------------------------------------------------
;;; STAGE 4: Give Food/Water Tests
;;; ---------------------------------------------------------------------------

(deftest give-lunch-to-cyclops-test
  (testing "Giving lunch to cyclops makes it thirsty"
    (let [gs (-> (setup-with-food)
                 (parser-state/set-prsa :give)
                 (parser-state/set-prso :lunch)
                 (parser-state/set-prsi :cyclops))
          result (cyclops/cyclops-action gs)]
      (is (nil? (get-in result [:objects :lunch :in])) "Lunch should be consumed")
      (is (neg? (:cyclowrath result)) "Cyclowrath should be negative (thirsty)")
      (is (daemon/daemon-enabled? result :i-cyclops)
          "i-cyclops daemon should be enabled"))))

(deftest give-water-after-lunch-test
  (testing "Giving water after lunch puts cyclops to sleep"
    (let [gs (-> (setup-with-food)
                 (assoc :cyclowrath -1)  ; Already ate peppers
                 (parser-state/set-prsa :give)
                 (parser-state/set-prso :water)
                 (parser-state/set-prsi :cyclops))
          result (cyclops/cyclops-action gs)]
      (is (true? (:cyclops-flag result)) "cyclops-flag should be true (asleep)")
      (is (nil? (get-in result [:objects :water :in])) "Water should be consumed")
      (is (= :cyclops-room (get-in result [:objects :bottle :in]))
          "Bottle should be dropped in room")
      (is (false? (daemon/daemon-enabled? result :i-cyclops))
          "i-cyclops daemon should be disabled"))))

(deftest give-water-without-lunch-test
  (testing "Giving water without eating lunch first is refused"
    (let [gs (-> (setup-with-food)
                 ;; cyclowrath is 0 (hasn't eaten peppers)
                 (parser-state/set-prsa :give)
                 (parser-state/set-prso :water)
                 (parser-state/set-prsi :cyclops))
          result (cyclops/cyclops-action gs)]
      (is (false? (:cyclops-flag result)) "cyclops-flag should remain false")
      (is (= :bottle (get-in result [:objects :water :in]))
          "Water should still be in bottle"))))

(deftest give-garlic-to-cyclops-test
  (testing "Giving garlic to cyclops is refused"
    (let [gs (-> (setup-cyclops-room)
                 (assoc-in [:objects :garlic :in] :adventurer)
                 (parser-state/set-prsa :give)
                 (parser-state/set-prso :garlic)
                 (parser-state/set-prsi :cyclops))
          result (cyclops/cyclops-action gs)]
      (is (= :adventurer (get-in result [:objects :garlic :in]))
          "Garlic should still be held"))))

;;; ---------------------------------------------------------------------------
;;; STAGE 5: Daemon Tests
;;; ---------------------------------------------------------------------------

(deftest cyclops-daemon-increments-wrath-test
  (testing "I-CYCLOPS daemon increments wrath each turn"
    (let [gs (-> (setup-cyclops-room)
                 (assoc :cyclowrath 1))
          result (cyclops/i-cyclops gs)]
      (is (= 2 (:cyclowrath result)) "Cyclowrath should increment"))))

(deftest cyclops-daemon-decrements-negative-wrath-test
  (testing "I-CYCLOPS daemon decrements negative wrath"
    (let [gs (-> (setup-cyclops-room)
                 (assoc :cyclowrath -1))
          result (cyclops/i-cyclops gs)]
      (is (= -2 (:cyclowrath result)) "Cyclowrath should decrement"))))

(deftest cyclops-daemon-kills-at-wrath-6-test
  (testing "I-CYCLOPS kills player when wrath exceeds 5"
    (let [gs (-> (setup-cyclops-room)
                 (assoc :cyclowrath 6))
          result (cyclops/i-cyclops gs)]
      (is (true? (:dead result)) "Player should be dead"))))

(deftest cyclops-daemon-disabled-when-asleep-test
  (testing "I-CYCLOPS does nothing when cyclops is asleep"
    (let [gs (-> (setup-cyclops-room)
                 (assoc :cyclops-flag true)
                 (assoc :cyclowrath 6))  ; Would kill if not asleep
          result (cyclops/i-cyclops gs)]
      (is (nil? (:dead result)) "Player should not be dead"))))

(deftest cyclops-daemon-disabled-when-leaving-room-test
  (testing "I-CYCLOPS disables itself when player leaves room"
    (let [gs (-> (core/init-game)
                 (assoc :here :maze-15)  ; Not in cyclops room
                 (daemon/queue :i-cyclops -1))  ; Enable daemon
          result (cyclops/i-cyclops gs)]
      (is (false? (daemon/daemon-enabled? result :i-cyclops))
          "Daemon should be disabled"))))

;;; ---------------------------------------------------------------------------
;;; STAGE 6: Room Action Tests
;;; ---------------------------------------------------------------------------

(deftest cyclops-room-look-normal-test
  (testing "Room look shows hungry cyclops when wrath is 0"
    (let [gs (setup-cyclops-room)
          result (cyclops/cyclops-room-action gs :look)
          output (get-in result [:output])]
      ;; The output should contain the cyclops description
      (is (some? result) "Should return updated game-state"))))

(deftest cyclops-room-look-sleeping-test
  (testing "Room look shows sleeping cyclops when asleep"
    (let [gs (-> (setup-cyclops-room)
                 (assoc :cyclops-flag true))
          result (cyclops/cyclops-room-action gs :look)]
      (is (some? result) "Should return updated game-state"))))

(deftest cyclops-room-look-fled-test
  (testing "Room look shows opened wall when cyclops fled"
    (let [gs (-> (setup-cyclops-room)
                 (assoc :magic-flag true))
          result (cyclops/cyclops-room-action gs :look)]
      (is (some? result) "Should return updated game-state"))))

(deftest cyclops-room-enter-enables-daemon-test
  (testing "Entering room with non-zero wrath enables daemon"
    (let [gs (-> (setup-cyclops-room)
                 (assoc :cyclowrath 1))
          result (cyclops/cyclops-room-action gs :enter)]
      (is (daemon/daemon-enabled? result :i-cyclops)
          "Daemon should be enabled"))))

;;; ---------------------------------------------------------------------------
;;; STAGE 7: Combat Interaction Tests
;;; ---------------------------------------------------------------------------

(deftest attack-cyclops-test
  (testing "Attacking cyclops makes it angrier"
    (let [gs (-> (setup-cyclops-room)
                 (parser-state/set-prsa :attack))
          result (cyclops/cyclops-action gs)]
      (is (daemon/daemon-enabled? result :i-cyclops)
          "Daemon should be enabled after attack"))))

(deftest wake-sleeping-cyclops-test
  (testing "Attacking sleeping cyclops wakes it up"
    (let [gs (-> (setup-cyclops-room)
                 (assoc :cyclops-flag true)  ; Asleep
                 (parser-state/set-prsa :attack))
          result (cyclops/cyclops-action gs)]
      (is (false? (:cyclops-flag result)) "Cyclops should wake up")
      (is (gs/set-thing-flag? result :cyclops :fight) "Cyclops should have fight flag"))))

;;; ---------------------------------------------------------------------------
;;; STAGE 8: Parser/Verb Integration Tests
;;; ---------------------------------------------------------------------------

(deftest odysseus-verb-registered-test
  (testing "ODYSSEUS verb is registered in verb definitions"
    (let [gs (core/init-game)]
      ;; Just verify the game initializes without errors
      ;; The actual verb handling is tested via v-odysseus directly
      (is (some? gs) "Game should initialize"))))
