(ns clork.loud-room-test
  "Tests for the Loud Room area and echo puzzle implementation."
  (:require [clojure.test :refer :all]
            [clork.core :as core]
            [clork.game-state :as gs]
            [clork.loud-room :as loud-room]
            [clork.parser.state :as parser-state]
            [clork.verbs-movement :as verbs-movement]
            [clork.debug.scenarios :as scenarios]))

;;; ---------------------------------------------------------------------------
;;; HELPER FUNCTIONS
;;; ---------------------------------------------------------------------------
;;; Note: These use the debug.scenarios module for consistent test setup.
;;; The scenarios module provides pre-configured game states with:
;;; - Player equipped with lamp and sword
;;; - Troll already dead
;;; - Grating open for underground access

(defn setup-loud-room
  "Set up game state with player in loud room (quiet state)."
  []
  (scenarios/loud-room-scenario :loud-room :quiet))

(defn setup-deep-canyon
  "Set up game state with player in deep canyon."
  []
  (scenarios/start-at :deep-canyon))

(defn setup-loud-room-with-water
  "Set up game state with player in loud room and reservoir full.
   (Gates open, low-tide false = deafening noise)"
  []
  (scenarios/loud-room-scenario :loud-room :loud))

(defn setup-loud-room-quiet
  "Set up game state with player in loud room but quiet.
   (Gates closed, low-tide true = no rushing water)"
  []
  (scenarios/loud-room-scenario :loud-room :quiet))

;;; ---------------------------------------------------------------------------
;;; STAGE 1: Room Definition Tests
;;; ---------------------------------------------------------------------------

(deftest loud-room-exists-test
  (testing "Loud room is defined with correct properties"
    (let [gs (core/init-game)
          room (gs/get-thing gs :loud-room)]
      (is (some? room) "Loud room should exist")
      (is (:action room) "Loud room should have an action handler"))))

(deftest loud-area-rooms-exist-test
  (testing "All loud room area rooms exist"
    (let [gs (core/init-game)]
      (is (gs/get-thing gs :loud-room) "loud-room should exist")
      (is (gs/get-thing gs :deep-canyon) "deep-canyon should exist")
      (is (gs/get-thing gs :damp-cave) "damp-cave should exist"))))

(deftest passage-rooms-exist-test
  (testing "Passage rooms exist"
    (let [gs (core/init-game)]
      (is (gs/get-thing gs :narrow-passage) "narrow-passage should exist")
      (is (gs/get-thing gs :engravings-cave) "engravings-cave should exist"))))

(deftest loud-room-exits-test
  (testing "Loud room has correct exits"
    (let [gs (core/init-game)
          room (gs/get-thing gs :loud-room)
          exits (:exits room)]
      (is (= :round-room (:west exits)) "West exit should go to round-room")
      (is (= :damp-cave (:east exits)) "East exit should go to damp-cave")
      (is (= :deep-canyon (:up exits)) "Up exit should go to deep-canyon"))))

(deftest deep-canyon-exits-test
  (testing "Deep canyon has correct exits"
    (let [gs (core/init-game)
          room (gs/get-thing gs :deep-canyon)
          exits (:exits room)]
      (is (= :loud-room (:down exits)) "Down exit should go to loud-room")
      (is (= :dam-room (:east exits)) "East exit should go to dam-room")
      (is (= :reservoir-south (:nw exits)) "NW exit should go to reservoir-south")
      (is (= :ns-passage (:sw exits)) "SW exit should go to ns-passage"))))

(deftest round-room-exits-updated-test
  (testing "Round room exits are correctly updated"
    (let [gs (core/init-game)
          room (gs/get-thing gs :round-room)
          exits (:exits room)]
      (is (= :loud-room (:east exits)) "East exit should go to loud-room")
      (is (= :narrow-passage (:south exits)) "South exit should go to narrow-passage")
      (is (= :engravings-cave (:se exits)) "SE exit should go to engravings-cave"))))

;;; ---------------------------------------------------------------------------
;;; STAGE 2: Object Definition Tests
;;; ---------------------------------------------------------------------------

(deftest platinum-bar-exists-test
  (testing "Platinum bar is defined correctly"
    (let [gs (core/init-game)
          bar (gs/get-thing gs :platinum-bar)]
      (is (some? bar) "Platinum bar should exist")
      (is (= :loud-room (:in bar)) "Platinum bar should be in loud-room")
      (is (gs/set-thing-flag? gs :platinum-bar :take) "Platinum bar should have take flag")
      (is (gs/set-thing-flag? gs :platinum-bar :sacred) "Platinum bar should start with sacred flag"))))

;;; ---------------------------------------------------------------------------
;;; STAGE 3: Room Quietness Tests
;;; ---------------------------------------------------------------------------

(deftest room-is-quiet-initial-test
  (testing "Loud room starts quiet (gates closed, low-tide true)"
    (let [gs (setup-loud-room)]
      (is (loud-room/room-is-quiet? gs) "Room should be quiet initially"))))

(deftest room-is-quiet-with-loud-flag-test
  (testing "Loud room is quiet when loud-flag is set"
    (let [gs (-> (setup-loud-room-with-water)
                 (assoc :loud-flag true))]
      (is (loud-room/room-is-quiet? gs) "Room should be quiet with loud-flag"))))

(deftest room-is-not-quiet-with-rushing-water-test
  (testing "Loud room is not quiet when water is rushing"
    (let [gs (setup-loud-room-with-water)]
      (is (not (loud-room/room-is-quiet? gs)) "Room should not be quiet with rushing water"))))

(deftest room-is-deafening-test
  (testing "Room is deafening when gates open and water high"
    (let [gs (setup-loud-room-with-water)]
      (is (loud-room/room-is-deafening? gs) "Room should be deafening"))))

(deftest room-is-not-deafening-when-quiet-test
  (testing "Room is not deafening when quiet"
    (let [gs (setup-loud-room-quiet)]
      (is (not (loud-room/room-is-deafening? gs)) "Room should not be deafening"))))

;;; ---------------------------------------------------------------------------
;;; STAGE 4: Echo Puzzle Tests
;;; ---------------------------------------------------------------------------

(deftest echo-in-loud-room-sets-flag-test
  (testing "Saying 'echo' in loud room sets loud-flag"
    (let [gs (setup-loud-room)
          result (loud-room/v-echo gs)]
      (is (:loud-flag result) "loud-flag should be set after saying echo"))))

(deftest echo-in-loud-room-removes-sacred-test
  (testing "Saying 'echo' in loud room removes sacred flag from platinum bar"
    (let [gs (setup-loud-room)
          _ (is (gs/set-thing-flag? gs :platinum-bar :sacred) "Bar should start sacred")
          result (loud-room/v-echo gs)]
      (is (not (gs/set-thing-flag? result :platinum-bar :sacred))
          "Platinum bar should no longer be sacred after echo"))))

(deftest echo-elsewhere-does-not-solve-puzzle-test
  (testing "Saying 'echo' elsewhere does not solve puzzle"
    (let [gs (-> (core/init-game)
                 (assoc :here :round-room))
          result (loud-room/v-echo gs)]
      (is (not (:loud-flag result)) "loud-flag should not be set")
      (is (gs/set-thing-flag? result :platinum-bar :sacred)
          "Platinum bar should still be sacred"))))

;;; ---------------------------------------------------------------------------
;;; STAGE 5: Room Description Tests
;;; ---------------------------------------------------------------------------

(deftest loud-room-description-quiet-test
  (testing "Loud room description when quiet"
    (let [gs (setup-loud-room-quiet)
          output (with-out-str (loud-room/loud-room-action gs :look))]
      (is (re-find #"eerie in its quietness" output)
          "Description should mention quietness"))))

(deftest loud-room-description-loud-test
  (testing "Loud room description when loud"
    (let [gs (-> (setup-loud-room)
                 (assoc :gates-open true)
                 (assoc :low-tide true))  ; Gates open but low-tide = loud but not deafening
          output (with-out-str (loud-room/loud-room-action gs :look))]
      (is (re-find #"deafeningly loud" output)
          "Description should mention loudness"))))

(deftest deep-canyon-description-loud-test
  (testing "Deep canyon description mentions rushing water when loud"
    (let [gs (-> (setup-deep-canyon)
                 (assoc :gates-open true)
                 (assoc :low-tide false))
          output (with-out-str (loud-room/deep-canyon-action gs :look))]
      (is (re-find #"loud roaring" output)
          "Description should mention loud roaring"))))

(deftest deep-canyon-description-flowing-test
  (testing "Deep canyon description mentions flowing water normally"
    (let [gs (-> (setup-deep-canyon)
                 (assoc :gates-open false)
                 (assoc :low-tide false))
          output (with-out-str (loud-room/deep-canyon-action gs :look))]
      (is (re-find #"flowing water" output)
          "Description should mention flowing water"))))

;;; ---------------------------------------------------------------------------
;;; STAGE 6: Integration Tests
;;; ---------------------------------------------------------------------------
;;; Note: Using scenarios module - player already has lamp and troll is dead

(deftest walk-to-loud-room-test
  (testing "Can walk from round-room to loud-room"
    (let [gs (-> (scenarios/start-at :round-room)
                 (assoc-in [:parser :prsa] :walk)
                 (assoc-in [:parser :prso] [:east]))
          final (verbs-movement/v-walk gs)]
      (is (= :loud-room (:here final)) "Should be in loud-room"))))

(deftest walk-deep-canyon-to-loud-room-test
  (testing "Can walk from deep-canyon to loud-room"
    (let [gs (-> (scenarios/start-at :deep-canyon)
                 (assoc-in [:parser :prsa] :walk)
                 (assoc-in [:parser :prso] [:down]))
          final (verbs-movement/v-walk gs)]
      (is (= :loud-room (:here final)) "Should be in loud-room"))))

(deftest echo-verb-handler-test
  (testing "Echo verb handler solves puzzle in loud room"
    (let [gs (setup-loud-room)
          result (loud-room/v-echo gs)]
      (is (:loud-flag result) "loud-flag should be set after echo verb"))))
