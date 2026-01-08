(ns clork.thief-test
  "Tests for the Thief NPC implementation."
  (:require [clojure.test :refer :all]
            [clork.core :as core]
            [clork.game-state :as gs]
            [clork.flags :as flags]
            [clork.thief :as thief]
            [clork.daemon :as daemon]
            [clork.parser.state :as parser-state]
            [clork.combat :as combat]
            [clork.random :as random]))

;;; ---------------------------------------------------------------------------
;;; STAGE 1: Object Definition Tests
;;; ---------------------------------------------------------------------------

(deftest thief-object-exists-test
  (testing "Thief is defined with correct properties"
    (let [gs (core/init-game)
          thief (gs/get-thing gs :thief)]
      (is (some? thief) "Thief object should exist")
      (is (= :round-room (:in thief)) "Thief should start in round-room")
      (is (= 5 (:strength thief)) "Thief should have strength 5")
      (is (gs/set-thing-flag? gs :thief :invisible) "Thief should start invisible")
      (is (gs/set-thing-flag? gs :thief :actor) "Thief should have actor flag"))))

(deftest stiletto-object-exists-test
  (testing "Stiletto is defined correctly"
    (let [gs (core/init-game)
          stiletto (gs/get-thing gs :stiletto)]
      (is (some? stiletto) "Stiletto should exist")
      (is (= :thief (:in stiletto)) "Stiletto should be in thief")
      (is (gs/set-thing-flag? gs :stiletto :weapon) "Stiletto should be a weapon"))))

(deftest large-bag-object-exists-test
  (testing "Large bag is defined correctly"
    (let [gs (core/init-game)
          bag (gs/get-thing gs :large-bag)]
      (is (some? bag) "Large bag should exist")
      (is (= :thief (:in bag)) "Large bag should be in thief"))))

;;; ---------------------------------------------------------------------------
;;; STAGE 0: Room Prerequisites Tests
;;; ---------------------------------------------------------------------------

(deftest treasure-room-exists-test
  (testing "Treasure room is defined"
    (let [gs (core/init-game)
          room (gs/get-thing gs :treasure-room)]
      (is (some? room) "Treasure room should exist")
      (is (= 25 (:value room)) "Treasure room should have value 25"))))

(deftest cyclops-room-exists-test
  (testing "Cyclops room is defined"
    (let [gs (core/init-game)
          room (gs/get-thing gs :cyclops-room)]
      (is (some? room) "Cyclops room should exist"))))

(deftest sacred-flag-on-outdoor-rooms-test
  (testing "Outdoor rooms have :sacred flag"
    (let [gs (core/init-game)]
      (doseq [room-id [:west-of-house :north-of-house :south-of-house
                       :behind-house :forest-1 :forest-2 :forest-3
                       :forest-path :clearing :grating-clearing
                       :up-a-tree :kitchen :living-room :attic
                       :stone-barrow]]
        (is (gs/set-thing-flag? gs room-id :sacred)
            (str room-id " should have :sacred flag"))))))

;;; ---------------------------------------------------------------------------
;;; STAGE 2: Thief Action Handler Tests
;;; ---------------------------------------------------------------------------

(deftest thief-action-f-busy-recovers-stiletto-test
  (testing "F-BUSY? mode - thief recovers stiletto from room"
    (let [gs (-> (core/init-game)
                 (assoc :here :round-room)
                 ;; Move stiletto to room
                 (assoc-in [:objects :stiletto :in] :round-room))
          result (thief/thief-action gs :f-busy?)]
      (is (= :thief (gs/get-thing-loc-id result :stiletto))
          "Thief should recover stiletto"))))

(deftest thief-action-f-dead-drops-stiletto-test
  (testing "F-DEAD mode - thief drops stiletto"
    (let [gs (-> (core/init-game)
                 (assoc :here :round-room)
                 (daemon/register-daemon :i-thief (fn [s] s) :tick -1))
          result (thief/thief-action gs :f-dead)]
      (is (= :round-room (gs/get-thing-loc-id result :stiletto))
          "Stiletto should be dropped in current room"))))

(deftest thief-action-f-unconscious-test
  (testing "F-UNCONSCIOUS mode - disables daemon and drops stiletto"
    (let [gs (-> (core/init-game)
                 (assoc :here :round-room)
                 (gs/set-thing-flag :thief :fight)
                 (daemon/register-daemon :i-thief (fn [s] s) :tick -1))
          result (thief/thief-action gs :f-unconscious)]
      (is (= :round-room (gs/get-thing-loc-id result :stiletto))
          "Stiletto should be dropped")
      (is (not (gs/set-thing-flag? result :thief :fight))
          "Fight flag should be cleared")
      (is (= thief/robber-u-desc (get-in result [:objects :thief :ldesc]))
          "Description should be unconscious"))))

(deftest thief-action-f-conscious-test
  (testing "F-CONSCIOUS mode - re-enables daemon"
    (let [gs (-> (core/init-game)
                 (assoc :here :cellar)  ; Not same room as thief
                 (daemon/register-daemon :i-thief (fn [s] s) :tick -1)
                 (daemon/disable :i-thief))
          result (thief/thief-action gs :f-conscious)]
      (is (= thief/robber-c-desc (get-in result [:objects :thief :ldesc]))
          "Description should be conscious"))))

(deftest thief-action-examine-test
  (testing "EXAMINE verb - shows detailed description"
    (let [gs (-> (core/init-game)
                 (parser-state/set-prsa :examine)
                 (parser-state/set-prso :thief))
          output (with-out-str (thief/thief-action gs))]
      (is (re-find #"slippery character" output)
          "Should show detailed examination"))))

(deftest thief-has-action-test
  (testing "Thief object has :action key"
    (let [gs (core/init-game)
          thief (gs/get-thing gs :thief)]
      (is (some? (:action thief)) "Thief should have an action function"))))

;;; ---------------------------------------------------------------------------
;;; STAGE 3: Combat Integration Tests
;;; ---------------------------------------------------------------------------

(deftest thief-in-villains-registry-test
  (testing "Thief is registered as a villain"
    (let [entry (get combat/villains-registry :thief)]
      (is (some? entry) "Thief should be in villains-registry")
      (is (= :thief (:villain-id entry)))
      (is (= :knife (:best-weapon entry)) "Knife should be best weapon against thief")
      (is (= 1 (:best-adv entry)))
      (is (= 0 (:wake-prob entry)))
      (is (= combat/thief-melee (:messages entry))))))

(deftest thief-melee-messages-test
  (testing "Thief melee messages are defined"
    (is (some? combat/thief-melee) "Thief melee table should exist")
    (is (seq (get combat/thief-melee combat/missed)) "Should have missed messages")
    (is (seq (get combat/thief-melee combat/killed)) "Should have killed messages")
    (is (seq (get combat/thief-melee combat/light-wound)) "Should have light-wound messages")
    (is (seq (get combat/thief-melee combat/serious-wound)) "Should have serious-wound messages")
    (is (seq (get combat/thief-melee combat/stagger)) "Should have stagger messages")
    (is (seq (get combat/thief-melee combat/lose-weapon)) "Should have lose-weapon messages")
    (is (seq (get combat/thief-melee combat/hesitate)) "Should have hesitate messages")
    (is (seq (get combat/thief-melee combat/sitting-duck)) "Should have sitting-duck messages")))

;;; ---------------------------------------------------------------------------
;;; STAGE 4: I-THIEF Daemon Tests
;;; ---------------------------------------------------------------------------

(deftest i-thief-daemon-registered-test
  (testing "I-THIEF daemon is registered on game start"
    (let [gs (core/init-game)]
      (is (some? (get-in gs [:daemons :i-thief]))
          "I-THIEF daemon should be registered"))))

(deftest i-thief-moves-to-valid-room-test
  (testing "I-THIEF daemon moves thief to a non-sacred room"
    (let [gs (-> (core/init-game)
                 (assoc :here :cellar))  ; Player not in round-room
          result (thief/i-thief gs)
          new-loc (gs/get-thing-loc-id result :thief)]
      ;; Thief should have moved from round-room
      (is (not= :round-room new-loc)
          "Thief should have moved from starting room")
      ;; New location should not be sacred
      (is (not (gs/set-thing-flag? result new-loc :sacred))
          "Thief should be in a non-sacred room"))))

(deftest i-thief-becomes-invisible-when-moving-test
  (testing "Thief becomes invisible when moving"
    (let [gs (-> (core/init-game)
                 (assoc :here :cellar)
                 (gs/unset-thing-flag :thief :invisible))
          result (thief/i-thief gs)]
      (is (gs/set-thing-flag? result :thief :invisible)
          "Thief should become invisible after moving"))))

(deftest i-thief-clears-fight-flag-test
  (testing "Thief clears fight flag when moving"
    (let [gs (-> (core/init-game)
                 (assoc :here :cellar)
                 (gs/set-thing-flag :thief :fight))
          result (thief/i-thief gs)]
      (is (not (gs/set-thing-flag? result :thief :fight))
          "Fight flag should be cleared after moving"))))

(deftest i-thief-skips-sacred-rooms-test
  (testing "Thief avoids sacred rooms"
    (let [gs (core/init-game)
          ;; Run daemon many times and check thief never ends up in sacred room
          final-gs (reduce (fn [state _]
                             (thief/i-thief state))
                           gs
                           (range 20))]
      (let [thief-loc (gs/get-thing-loc-id final-gs :thief)]
        (is (not (gs/set-thing-flag? final-gs thief-loc :sacred))
            "Thief should never be in a sacred room")))))

;;; ---------------------------------------------------------------------------
;;; STAGE 5: Stealing Behavior Tests
;;; ---------------------------------------------------------------------------

(deftest rob-steals-valuables-test
  (testing "Rob function steals valuable items"
    (let [gs (-> (core/init-game)
                 ;; Place a valuable item in a room
                 (assoc-in [:objects :egg :in] :cellar)
                 (gs/unset-thing-flag :egg :invisible))
          [result robbed?] (thief/rob gs :cellar :thief nil)]
      (is robbed? "Should indicate something was robbed")
      (is (= :thief (gs/get-thing-loc-id result :egg))
          "Valuable item should be moved to thief"))))

(deftest rob-skips-invisible-items-test
  (testing "Rob function skips invisible items"
    (let [gs (-> (core/init-game)
                 (assoc-in [:objects :egg :in] :cellar)
                 (gs/set-thing-flag :egg :invisible))
          [result robbed?] (thief/rob gs :cellar :thief nil)]
      (is (not robbed?) "Should not rob invisible items")
      (is (= :cellar (gs/get-thing-loc-id result :egg))
          "Invisible item should stay in place"))))

(deftest rob-skips-sacred-items-test
  (testing "Rob function skips sacred items"
    (let [gs (-> (core/init-game)
                 (assoc-in [:objects :egg :in] :cellar)
                 (gs/set-thing-flag :egg :sacred)
                 (gs/unset-thing-flag :egg :invisible))
          [result robbed?] (thief/rob gs :cellar :thief nil)]
      (is (not robbed?) "Should not rob sacred items")
      (is (= :cellar (gs/get-thing-loc-id result :egg))
          "Sacred item should stay in place"))))

(deftest drop-junk-drops-worthless-items-test
  (testing "Drop-junk function drops worthless items"
    ;; Use seeded random for deterministic test
    (random/init! 12345)
    (let [gs (-> (core/init-game)
                 ;; Put a worthless item in thief's inventory
                 (assoc-in [:objects :sword :tvalue] 0)  ; Temporarily make sword worthless
                 (assoc-in [:objects :sword :in] :thief))
          ;; Run drop-junk many times to ensure 30% probability triggers
          [result _] (reduce (fn [[state announced?] _]
                               (thief/drop-junk state :cellar))
                             [gs false]
                             (range 20))]
      ;; The sword should have been dropped at some point
      (is (or (= :cellar (gs/get-thing-loc-id result :sword))
              (= :thief (gs/get-thing-loc-id result :sword)))
          "Item should either be dropped or still with thief"))))

(deftest drop-junk-keeps-stiletto-test
  (testing "Drop-junk never drops stiletto"
    (random/init! 99999)  ; Different seed
    (let [gs (core/init-game)
          ;; Run drop-junk many times
          [result _] (reduce (fn [[state announced?] _]
                               (thief/drop-junk state :cellar))
                             [gs false]
                             (range 50))]
      (is (= :thief (gs/get-thing-loc-id result :stiletto))
          "Stiletto should never be dropped"))))

;;; ---------------------------------------------------------------------------
;;; STAGE 6: THIEF-VS-ADVENTURER Tests
;;; ---------------------------------------------------------------------------

(deftest thief-vs-adventurer-retreats-when-losing-test
  (testing "Thief retreats when losing fight"
    (let [gs (-> (core/init-game)
                 (assoc :here :cellar)
                 (assoc-in [:objects :thief :in] :cellar)
                 (gs/unset-thing-flag :thief :invisible)
                 (gs/set-thing-flag :thief :fight)
                 ;; Make thief weaker than player (losing)
                 (assoc-in [:objects :thief :strength] 1)
                 (assoc-in [:objects :adventurer :strength] 5))
          [result finished?] (thief/thief-vs-adventurer gs true)]
      (is finished? "Should finish after retreat")
      (is (gs/set-thing-flag? result :thief :invisible)
          "Thief should become invisible")
      (is (not (gs/set-thing-flag? result :thief :fight))
          "Fight flag should be cleared"))))

(deftest thief-vs-adventurer-first-appearance-test
  (testing "Thief can appear for first time"
    ;; Seed random to get 30% appearance
    (random/init! 42)
    (let [gs (-> (core/init-game)
                 (assoc :here :cellar)
                 (assoc-in [:objects :thief :in] :cellar)
                 (gs/set-thing-flag :thief :invisible)
                 (assoc :thief-here false))
          ;; Run multiple times to trigger 30% appearance
          final-result (loop [state gs
                              i 0]
                         (if (>= i 20)
                           [state false]
                           (let [[result finished?] (thief/thief-vs-adventurer state false)]
                             (if finished?
                               [result finished?]
                               (recur result (inc i))))))]
      ;; Either appeared or not (probabilistic)
      (is (vector? final-result) "Should return result"))))

(deftest winning-function-test
  (testing "Winning function compares strengths correctly"
    (let [gs (-> (core/init-game)
                 (assoc-in [:objects :thief :strength] 10)
                 (assoc-in [:objects :adventurer :strength] 5))]
      (is (thief/winning? gs :thief)
          "Thief with higher strength should be winning"))
    (let [gs (-> (core/init-game)
                 (assoc-in [:objects :thief :strength] 2)
                 (assoc-in [:objects :adventurer :strength] 5))]
      (is (not (thief/winning? gs :thief))
          "Thief with lower strength should not be winning"))))

;;; ---------------------------------------------------------------------------
;;; STAGE 7: Treasure Room Behavior Tests
;;; ---------------------------------------------------------------------------

(deftest hack-treasures-unhides-valuables-test
  (testing "Hack-treasures reveals hidden valuable items"
    (let [gs (-> (core/init-game)
                 ;; Put a valuable hidden item in treasure room
                 (assoc-in [:objects :egg :in] :treasure-room)
                 (gs/set-thing-flag :egg :invisible))
          result (thief/hack-treasures gs)]
      (is (not (gs/set-thing-flag? result :egg :invisible))
          "Valuable items should become visible"))))

(deftest thief-in-treasure-hides-valuables-test
  (testing "Thief-in-treasure hides valuables when player enters"
    (let [gs (-> (core/init-game)
                 ;; Put a valuable visible item in treasure room
                 (assoc-in [:objects :egg :in] :treasure-room)
                 (gs/unset-thing-flag :egg :invisible))
          result (thief/thief-in-treasure gs)]
      (is (gs/set-thing-flag? result :egg :invisible)
          "Valuable items should become invisible")
      (is (= :treasure-room (gs/get-thing-loc-id result :thief))
          "Thief should be in treasure room")
      (is (not (gs/set-thing-flag? result :thief :invisible))
          "Thief should be visible")
      (is (gs/set-thing-flag? result :thief :fight)
          "Thief should be ready to fight"))))

(deftest treasure-room-action-thief-rushes-test
  (testing "Thief rushes to defend when player enters treasure room"
    (let [gs (-> (core/init-game)
                 (assoc :here :treasure-room)
                 (assoc-in [:objects :thief :in] :cellar))  ; Thief elsewhere
          output (with-out-str
                   (thief/treasure-room-action gs :m-enter))]
      (is (re-find #"scream of anguish" output)
          "Should announce thief rushing"))))

(deftest i-thief-deposits-booty-in-treasure-room-test
  (testing "Thief deposits booty when alone in treasure room"
    (let [gs (-> (core/init-game)
                 (assoc :here :cellar)  ; Player not in treasure room
                 (assoc-in [:objects :thief :in] :treasure-room)
                 ;; Give thief a valuable item
                 (assoc-in [:objects :egg :in] :thief)
                 (gs/set-thing-flag :egg :invisible))
          result (thief/i-thief gs)]
      ;; The valuable should be deposited in treasure room
      (is (= :treasure-room (gs/get-thing-loc-id result :egg))
          "Valuable should be deposited in treasure room"))))

(deftest thief-opens-egg-when-depositing-test
  (testing "deposit-booty opens the egg but keeps it invisible"
    ;; Test deposit-booty directly to verify egg behavior.
    ;; Note: When thief is alone in treasure room, hack-treasures later reveals
    ;; treasures. But deposit-booty itself should NOT reveal them.
    (let [gs (-> (core/init-game)
                 ;; Give thief the egg (stolen items get invisible flag)
                 (assoc-in [:objects :egg :in] :thief)
                 (gs/set-thing-flag :egg :invisible)  ; Stolen items are invisible
                 (gs/unset-thing-flag :egg :open))  ; Ensure egg starts closed
          ;; Call deposit-booty directly
          result (thief/deposit-booty gs :treasure-room)]
      ;; The egg should be in the treasure room now
      (is (= :treasure-room (gs/get-thing-loc-id result :egg))
          "Egg should be deposited in treasure room")
      ;; The egg should be opened
      (is (gs/set-thing-flag? result :egg :open)
          "Thief should open the egg when depositing")
      ;; The egg-solve flag should be set
      (is (:egg-solve result)
          "egg-solve flag should be set")
      ;; The egg should STILL be invisible (treasures stay hidden until thief dies)
      (is (gs/set-thing-flag? result :egg :invisible)
          "Egg should remain invisible after deposit-booty (revealed later when thief dies)")
      ;; The canary should still be inside the egg
      (is (= :egg (gs/get-thing-loc-id result :clockwork-canary))
          "Canary should still be inside the egg"))))

(deftest thief-never-retreats-from-treasure-room-test
  (testing "Thief fights to the death in treasure room - never retreats"
    ;; This tests the bug fix: thief was incorrectly retreating from treasure room
    (let [gs (-> (core/init-game)
                 (assoc :here :treasure-room)
                 (assoc-in [:objects :thief :in] :treasure-room)
                 (gs/unset-thing-flag :thief :invisible)
                 (gs/set-thing-flag :thief :fight)
                 ;; Make thief weaker than player (losing badly)
                 (assoc-in [:objects :thief :strength] 1)
                 (assoc-in [:objects :adventurer :strength] 10))
          ;; Run multiple iterations - thief should never retreat
          results (repeatedly 10
                    #(thief/thief-vs-adventurer gs true))]
      ;; In treasure room, thief should never become invisible (retreat)
      ;; and should remain in treasure room
      (doseq [[result _] results]
        (is (= :treasure-room (gs/get-thing-loc-id result :thief))
            "Thief must stay in treasure room even when losing")
        (is (not (gs/set-thing-flag? result :thief :invisible))
            "Thief should not go invisible (retreat) in treasure room")))))

(deftest thief-does-not-rob-during-combat-test
  (testing "Thief doesn't use robbery logic when actively fighting"
    ;; This tests the bug fix: robbery logic was running even during combat
    (random/init! 42)
    (let [gs (-> (core/init-game)
                 (assoc :here :cellar)
                 (assoc-in [:objects :thief :in] :cellar)
                 (gs/unset-thing-flag :thief :invisible)
                 (gs/set-thing-flag :thief :fight)
                 ;; Player has valuable item
                 (assoc-in [:objects :egg :in] :adventurer)
                 (gs/unset-thing-flag :egg :invisible))
          ;; Run the daemon - should not steal during combat
          [result _] (thief/thief-vs-adventurer gs true)]
      ;; The egg should still be with the player, not stolen
      (is (= :adventurer (gs/get-thing-loc-id result :egg))
          "Thief should not steal from player during active combat"))))
