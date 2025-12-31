(ns clork.sword-test
  "Tests for sword glowing mechanic."
  (:require [clojure.test :refer [deftest testing is]]
            [clork.sword :as sword]
            [clork.game-state :as gs]
            [clork.flags :as flags]
            [clork.core :as core]
            [clork.daemon :as daemon]))

;;; ---------------------------------------------------------------------------
;;; Test helper
;;; ---------------------------------------------------------------------------

(defn test-game-state
  "Create a minimal test game state with rooms and objects."
  []
  (-> (gs/initial-game-state)
      ;; Add minimal room definitions
      (gs/add-rooms
       [{:id :west-of-house
         :desc "West of House"
         :flags #{:lit}
         :exits {:east :living-room}}
        {:id :living-room
         :desc "Living Room"
         :flags #{:lit}
         :exits {:west :west-of-house :down :cellar}}
        {:id :cellar
         :desc "Cellar"
         :flags #{}
         :exits {:up :living-room :north :troll-room}}
        {:id :troll-room
         :desc "Troll Room"
         :flags #{}
         :exits {:south :cellar}}])
      ;; Add minimal object definitions
      (gs/add-objects
       [{:id :adventurer
         :flags (flags/flags :ndesc :invisible :sacred :actor)
         :in :living-room}
        {:id :sword
         :in :living-room
         :flags (flags/flags :take :weapon)
         :tvalue 0}
        {:id :troll
         :in :troll-room
         :flags (flags/flags :actor)
         :strength 2}])
      ;; Register the sword daemon
      (daemon/register-daemon :i-sword sword/i-sword :tick -1)))

;;; ---------------------------------------------------------------------------
;;; infested? tests
;;; ---------------------------------------------------------------------------

(deftest infested?-empty-room-test
  (testing "Empty room is not infested"
    (let [gs (test-game-state)]
      (is (not (sword/infested? gs :living-room))))))

(deftest infested?-room-with-invisible-actor-test
  (testing "Room with invisible actor is not infested"
    ;; The adventurer has :actor but also :invisible
    (let [gs (test-game-state)]
      (is (not (sword/infested? gs :living-room))))))

(deftest infested?-room-with-visible-actor-test
  (testing "Room with visible actor (troll) is infested"
    (let [gs (test-game-state)]
      (is (sword/infested? gs :troll-room)))))

;;; ---------------------------------------------------------------------------
;;; adjacent-rooms tests
;;; ---------------------------------------------------------------------------

(deftest adjacent-rooms-test
  (testing "Returns all reachable adjacent rooms"
    (let [gs (test-game-state)]
      (is (= #{:cellar :west-of-house}
             (set (sword/adjacent-rooms gs :living-room)))))))

(deftest adjacent-rooms-cellar-test
  (testing "Cellar is adjacent to troll room"
    (let [gs (test-game-state)]
      (is (contains? (set (sword/adjacent-rooms gs :cellar)) :troll-room)))))

;;; ---------------------------------------------------------------------------
;;; calculate-glow-level tests
;;; ---------------------------------------------------------------------------

(deftest glow-level-no-enemies-test
  (testing "Glow level 0 when no enemies nearby"
    (let [gs (-> (test-game-state)
                 (assoc :here :west-of-house))]
      (is (= 0 (sword/calculate-glow-level gs))))))

(deftest glow-level-adjacent-enemy-test
  (testing "Glow level 1 when enemy in adjacent room"
    (let [gs (-> (test-game-state)
                 (assoc :here :cellar))]
      (is (= 1 (sword/calculate-glow-level gs))))))

(deftest glow-level-same-room-enemy-test
  (testing "Glow level 2 when enemy in same room"
    (let [gs (-> (test-game-state)
                 (assoc :here :troll-room))]
      (is (= 2 (sword/calculate-glow-level gs))))))

;;; ---------------------------------------------------------------------------
;;; i-sword daemon tests
;;; ---------------------------------------------------------------------------

(deftest i-sword-no-change-test
  (testing "Daemon doesn't print message when glow level unchanged"
    (let [gs (-> (test-game-state)
                 (assoc :here :west-of-house)
                 (assoc-in [:objects :sword :in] :adventurer)
                 (assoc-in [:objects :sword :tvalue] 0))
          output (with-out-str (sword/i-sword gs))]
      ;; Output should be empty (no message)
      (is (empty? output)))))

(deftest i-sword-faint-glow-test
  (testing "Daemon prints faint glow message when entering adjacent room"
    (let [gs (-> (test-game-state)
                 (assoc :here :cellar)
                 (assoc-in [:objects :sword :in] :adventurer)
                 (assoc-in [:objects :sword :tvalue] 0))
          result (sword/i-sword gs)
          output (with-out-str (sword/i-sword gs))]
      ;; Glow level should be updated to 1
      (is (= 1 (get-in result [:objects :sword :tvalue])))
      ;; Output should contain faint glow message
      (is (re-find #"faint blue glow" output)))))

(deftest i-sword-bright-glow-test
  (testing "Daemon prints bright glow message when entering enemy room"
    (let [gs (-> (test-game-state)
                 (assoc :here :troll-room)
                 (assoc-in [:objects :sword :in] :adventurer)
                 (assoc-in [:objects :sword :tvalue] 0))
          result (sword/i-sword gs)
          output (with-out-str (sword/i-sword gs))]
      ;; Glow level should be updated to 2
      (is (= 2 (get-in result [:objects :sword :tvalue])))
      ;; Output should contain bright glow message
      (is (re-find #"very brightly" output)))))

(deftest i-sword-stops-glowing-test
  (testing "Daemon prints stop glowing message when leaving enemy area"
    (let [gs (-> (test-game-state)
                 (assoc :here :west-of-house)
                 (assoc-in [:objects :sword :in] :adventurer)
                 (assoc-in [:objects :sword :tvalue] 2))
          result (sword/i-sword gs)
          output (with-out-str (sword/i-sword gs))]
      ;; Glow level should be updated to 0
      (is (= 0 (get-in result [:objects :sword :tvalue])))
      ;; Output should contain stop glowing message
      (is (re-find #"no longer glowing" output)))))

(deftest i-sword-not-carried-test
  (testing "Daemon does nothing when sword not in inventory"
    (let [gs (-> (test-game-state)
                 (assoc :here :troll-room))
          result (sword/i-sword gs)]
      ;; State should be unchanged
      (is (= gs result)))))

;;; ---------------------------------------------------------------------------
;;; sword-action tests
;;; ---------------------------------------------------------------------------

(deftest sword-action-examine-no-glow-test
  (testing "EXAMINE with no glow returns nil (default examine)"
    (let [gs (-> (test-game-state)
                 (assoc-in [:parser :prsa] :examine)
                 (assoc-in [:objects :sword :tvalue] 0))]
      (is (nil? (sword/sword-action gs))))))

(deftest sword-action-examine-faint-glow-test
  (testing "EXAMINE with faint glow describes it"
    (let [gs (-> (test-game-state)
                 (assoc-in [:parser :prsa] :examine)
                 (assoc-in [:objects :sword :tvalue] 1))
          output (with-out-str (sword/sword-action gs))]
      (is (re-find #"faint blue glow" output)))))

(deftest sword-action-examine-bright-glow-test
  (testing "EXAMINE with bright glow describes it"
    (let [gs (-> (test-game-state)
                 (assoc-in [:parser :prsa] :examine)
                 (assoc-in [:objects :sword :tvalue] 2))
          output (with-out-str (sword/sword-action gs))]
      (is (re-find #"very brightly" output)))))

;;; ---------------------------------------------------------------------------
;;; Integration test with full game state
;;; ---------------------------------------------------------------------------

(deftest sword-glow-integration-test
  (testing "Sword glows correctly through full game flow"
    (let [gs (core/init-game)
          ;; Move sword to player's inventory
          gs (assoc-in gs [:objects :sword :in] :adventurer)
          ;; Start in cellar (adjacent to troll)
          gs (assoc gs :here :cellar)
          ;; Run the daemon
          gs (daemon/clocker gs)]
      ;; Sword should now have tvalue 1 (faint glow)
      (is (= 1 (get-in gs [:objects :sword :tvalue])))
      ;; Move to troll room
      (let [gs (assoc gs :here :troll-room)
            gs (daemon/clocker gs)]
        ;; Sword should now have tvalue 2 (bright glow)
        (is (= 2 (get-in gs [:objects :sword :tvalue])))))))
