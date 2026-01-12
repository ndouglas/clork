(ns clork.planner2.goals-test
  "Integration tests for the goals module.
   Tests goal satisfaction and decomposition against real game state."
  (:require [clojure.test :refer :all]
            [clork.planner2.goals :as goals]
            [clork.planner2.observe :as obs]
            [clork.core :as core]
            [clork.game-state :as gs]))

;;; ---------------------------------------------------------------------------
;;; HELPER FUNCTIONS
;;; ---------------------------------------------------------------------------

(defn move-object-to
  "Move an object to a container (room, player, or other object)."
  [game-state obj-id dest-id]
  (assoc-in game-state [:objects obj-id :in] dest-id))

(defn set-obj-flag
  "Set a flag on an object."
  [game-state obj-id flag]
  (gs/set-flag game-state :objects obj-id flag))

;;; ---------------------------------------------------------------------------
;;; TEST FIXTURES
;;; ---------------------------------------------------------------------------

(defn fresh-game []
  (core/init-game))

;;; ---------------------------------------------------------------------------
;;; GOAL CONSTRUCTOR TESTS
;;; ---------------------------------------------------------------------------

(deftest goal-constructors-test
  (testing "at-room constructor"
    (let [goal (goals/at-room :kitchen)]
      (is (= :at-room (:type goal)))
      (is (= :kitchen (:room goal)))))

  (testing "have-item constructor"
    (let [goal (goals/have-item :sword)]
      (is (= :have-item (:type goal)))
      (is (= :sword (:item goal)))))

  (testing "item-deposited constructor"
    (let [goal (goals/item-deposited :egg)]
      (is (= :item-deposited (:type goal)))
      (is (= :egg (:item goal)))))

  (testing "kill-enemy constructor"
    (let [goal (goals/kill-enemy :troll)]
      (is (= :kill-enemy (:type goal)))
      (is (= :troll (:enemy goal)))))

  (testing "lantern-on constructor"
    (let [goal (goals/lantern-on)]
      (is (= :lantern-on (:type goal)))))

  (testing "win constructor"
    (let [goal (goals/win)]
      (is (= :win (:type goal))))))

;;; ---------------------------------------------------------------------------
;;; GOAL SATISFACTION TESTS - Initial State
;;; ---------------------------------------------------------------------------

(deftest goal-satisfied-initial-state-test
  (let [gs (fresh-game)]

    (testing "at-room - player starts west of house"
      (is (goals/goal-satisfied? gs (goals/at-room :west-of-house)))
      (is (not (goals/goal-satisfied? gs (goals/at-room :kitchen)))))

    (testing "have-item - player starts with nothing"
      (is (not (goals/goal-satisfied? gs (goals/have-item :sword))))
      (is (not (goals/goal-satisfied? gs (goals/have-item :brass-lantern)))))

    (testing "lantern-on - lantern starts off"
      (is (not (goals/goal-satisfied? gs (goals/lantern-on)))))

    (testing "kill-enemy - enemies start alive"
      (is (not (goals/goal-satisfied? gs (goals/kill-enemy :troll))))
      (is (not (goals/goal-satisfied? gs (goals/kill-enemy :cyclops)))))

    (testing "item-deposited - no treasures deposited initially"
      (is (not (goals/goal-satisfied? gs (goals/item-deposited :egg))))
      (is (not (goals/goal-satisfied? gs (goals/item-deposited :painting)))))

    (testing "all-treasures-deposited - not initially"
      (is (not (goals/goal-satisfied? gs (goals/all-treasures-deposited)))))

    (testing "win - not initially"
      (is (not (goals/goal-satisfied? gs (goals/win)))))))

;;; ---------------------------------------------------------------------------
;;; GOAL SATISFACTION TESTS - Manipulated State
;;; ---------------------------------------------------------------------------

(deftest goal-satisfied-after-teleport-test
  (let [gs (-> (fresh-game)
               (assoc :here :kitchen))]
    (testing "at-room after teleport"
      (is (goals/goal-satisfied? gs (goals/at-room :kitchen)))
      (is (not (goals/goal-satisfied? gs (goals/at-room :west-of-house)))))))

(deftest goal-satisfied-after-take-test
  (let [gs (fresh-game)
        ;; Manually move player to living room and give them the lantern
        gs-with-lantern (-> gs
                            (assoc :here :living-room)
                            (move-object-to :brass-lantern :player))]
    (testing "have-item after taking"
      (is (goals/goal-satisfied? gs-with-lantern (goals/have-item :brass-lantern))))))

(deftest goal-satisfied-lantern-on-test
  (let [gs (fresh-game)
        ;; Give lantern and turn it on
        gs-with-lantern (-> gs
                            (move-object-to :brass-lantern :player)
                            (set-obj-flag :brass-lantern :on))]
    (testing "lantern-on after turning on"
      (is (goals/goal-satisfied? gs-with-lantern (goals/lantern-on))))))

(deftest goal-satisfied-container-open-test
  (let [gs (fresh-game)
        ;; Open the trophy case
        gs-case-open (set-obj-flag gs :trophy-case :open)]
    (testing "container-open after opening"
      (is (goals/goal-satisfied? gs-case-open (goals/container-open :trophy-case))))))

;;; ---------------------------------------------------------------------------
;;; GOAL DECOMPOSITION TESTS
;;; ---------------------------------------------------------------------------

(deftest decompose-have-item-test
  (let [gs (fresh-game)]

    (testing "have-item when item is far away - decompose to at-room"
      ;; Sword is in living room, player at west-of-house
      (let [decomp (goals/decompose-goal gs (goals/have-item :sword))]
        (is (some? decomp))
        (is (= :at-room (:type (first decomp))))))

    (testing "have-item when already have it - no decomposition"
      (let [gs-with-item (move-object-to gs :sword :player)
            decomp (goals/decompose-goal gs-with-item (goals/have-item :sword))]
        (is (nil? decomp))))))

(deftest decompose-item-deposited-test
  (let [gs (fresh-game)]

    (testing "item-deposited when don't have item - decompose to have-item"
      (let [decomp (goals/decompose-goal gs (goals/item-deposited :egg))]
        (is (= :have-item (:type (first decomp))))
        (is (= :egg (:item (first decomp))))))

    (testing "item-deposited when have item but not at living room"
      (let [gs-with-egg (move-object-to gs :egg :player)
            decomp (goals/decompose-goal gs-with-egg (goals/item-deposited :egg))]
        (is (= :at-room (:type (first decomp))))
        (is (= :living-room (:room (first decomp))))))

    (testing "item-deposited when at living room with item but case closed"
      (let [gs-ready (-> gs
                         (move-object-to :egg :player)
                         (assoc :here :living-room))
            decomp (goals/decompose-goal gs-ready (goals/item-deposited :egg))]
        (is (= :container-open (:type (first decomp))))
        (is (= :trophy-case (:container (first decomp))))))

    (testing "item-deposited when all prerequisites met - no decomposition"
      (let [gs-ready (-> gs
                         (move-object-to :egg :player)
                         (assoc :here :living-room)
                         (set-obj-flag :trophy-case :open))
            decomp (goals/decompose-goal gs-ready (goals/item-deposited :egg))]
        ;; Ready to deposit, no further decomposition
        (is (nil? decomp))))))

(deftest decompose-lantern-on-test
  (let [gs (fresh-game)]

    (testing "lantern-on when don't have lantern - decompose to have-item"
      (let [decomp (goals/decompose-goal gs (goals/lantern-on))]
        (is (= :have-item (:type (first decomp))))
        (is (= :brass-lantern (:item (first decomp))))))

    (testing "lantern-on when have lantern - no decomposition (ready to act)"
      (let [gs-with-lantern (move-object-to gs :brass-lantern :player)
            decomp (goals/decompose-goal gs-with-lantern (goals/lantern-on))]
        (is (nil? decomp))))))

(deftest decompose-kill-enemy-test
  (let [gs (fresh-game)]

    (testing "kill-enemy without weapon - decompose to have-item sword"
      (let [decomp (goals/decompose-goal gs (goals/kill-enemy :troll))]
        (is (= :have-item (:type (first decomp))))
        (is (= :sword (:item (first decomp))))))

    (testing "kill-enemy with sword in dark room - needs light"
      ;; Troll room is dark, so need light before fighting
      (let [gs-armed (-> gs
                         (move-object-to :sword :player)
                         (assoc :here :troll-room))
            decomp (goals/decompose-goal gs-armed (goals/kill-enemy :troll))]
        (is (= :lantern-on (:type (first decomp))))))

    (testing "kill-enemy with sword and light but wrong room"
      (let [gs-ready (-> gs
                         (move-object-to :sword :player)
                         (move-object-to :brass-lantern :player)
                         (set-obj-flag :brass-lantern :on))
            decomp (goals/decompose-goal gs-ready (goals/kill-enemy :troll))]
        (is (= :at-room (:type (first decomp))))
        (is (= :troll-room (:room (first decomp))))))))

;;; ---------------------------------------------------------------------------
;;; GOAL->STRING TESTS
;;; ---------------------------------------------------------------------------

(deftest goal-to-string-test
  (testing "Goal descriptions are readable"
    (is (= "Go to kitchen" (goals/goal->string (goals/at-room :kitchen))))
    (is (= "Get sword" (goals/goal->string (goals/have-item :sword))))
    (is (= "Kill troll" (goals/goal->string (goals/kill-enemy :troll))))
    (is (= "Turn on lantern" (goals/goal->string (goals/lantern-on))))
    (is (= "Deposit egg" (goals/goal->string (goals/item-deposited :egg))))
    (is (= "Win the game" (goals/goal->string (goals/win))))))

;;; ---------------------------------------------------------------------------
;;; GOAL PRIORITY TESTS
;;; ---------------------------------------------------------------------------

(deftest goal-priority-test
  (testing "Safety goals have highest priority"
    (is (< (goals/goal-priority (goals/lantern-on))
           (goals/goal-priority (goals/at-room :kitchen)))))

  (testing "Navigation before acquisition"
    (is (< (goals/goal-priority (goals/at-room :kitchen))
           (goals/goal-priority (goals/have-item :sword)))))

  (testing "Win has lowest priority"
    (is (> (goals/goal-priority (goals/win))
           (goals/goal-priority (goals/have-item :sword))))))

;;; ---------------------------------------------------------------------------
;;; TREASURES SET TESTS
;;; ---------------------------------------------------------------------------

(deftest treasures-set-test
  (testing "Treasures set contains expected items"
    (is (contains? goals/treasures :egg))
    (is (contains? goals/treasures :painting))
    (is (contains? goals/treasures :pot-of-gold))
    (is (contains? goals/treasures :platinum-bar)))

  (testing "Treasures set has expected count"
    ;; Zork I has 19 treasures
    (is (= 19 (count goals/treasures)))))
