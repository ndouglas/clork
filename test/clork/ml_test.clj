(ns clork.ml-test
  (:require [clojure.test :refer :all]
            [clork.ml :as ml]
            [clork.game-state :as gs]
            [clork.rooms :as rooms]
            [clork.objects :as objects]
            [clork.verb-defs :as verb-defs]
            [clork.daemon :as daemon]
            [clork.sword :as sword]
            [clork.combat :as combat]))

(defn init-test-state
  "Create a fresh game state for testing."
  []
  (let [base (gs/initial-game-state)
        with-rooms (gs/add-rooms base rooms/all-rooms)
        with-objects (gs/add-objects with-rooms objects/all-objects)]
    ;; Register object vocabulary
    (verb-defs/register-object-vocabulary! (:objects with-objects))
    ;; Set initial room as lit
    (gs/set-here-flag with-objects :lit)))

(deftest test-get-visible-objects
  (testing "can get visible objects in starting room"
    (let [state (init-test-state)
          visible (ml/get-visible-objects state)]
      (is (vector? visible))
      (is (pos? (count visible)))
      ;; Should see the mailbox at west-of-house
      (is (some #(= :mailbox (:id %)) visible)))))

(deftest test-get-inventory
  (testing "inventory starts empty"
    (let [state (init-test-state)
          inv (ml/get-inventory state)]
      (is (vector? inv))
      (is (empty? inv)))))

(deftest test-get-available-exits
  (testing "can get exits from starting room"
    (let [state (init-test-state)
          exits (ml/get-available-exits state)]
      (is (map? exits))
      (is (pos? (count exits)))
      ;; West-of-house has north, south, west, ne exits
      (is (contains? exits :north))
      (is (contains? exits :south)))))

(deftest test-valid-actions
  (testing "valid-actions returns structured data"
    (let [state (init-test-state)
          actions (ml/valid-actions state)]
      (is (map? actions))
      (is (contains? actions :meta-verbs))
      (is (contains? actions :movement))
      (is (contains? actions :object-actions))
      (is (contains? actions :lit?))
      ;; Meta verbs should include :look
      (is (some #(= :look %) (:meta-verbs actions)))
      ;; Should have some movement options
      (is (pos? (count (get-in actions [:movement :directions]))))
      ;; Should have some object actions
      (is (pos? (count (:object-actions actions)))))))

(deftest test-action-list
  (testing "action-list returns flat list of executable actions"
    (let [state (init-test-state)
          actions (ml/action-list state)]
      (is (vector? actions))
      (is (pos? (count actions)))
      ;; Each action should have a :verb key
      (is (every? :verb actions))
      ;; Should include meta verbs
      (is (some #(= :look (:verb %)) actions))
      ;; Should include movement
      (is (some #(= :go (:verb %)) actions)))))

(deftest test-state-snapshot
  (testing "state-snapshot returns JSON-ready structure"
    (let [state (init-test-state)
          snapshot (ml/state-snapshot state)]
      (is (map? snapshot))
      (is (= 0 (:score snapshot)))
      (is (= 350 (:max-score snapshot)))
      (is (contains? snapshot :room))
      (is (contains? snapshot :valid-actions))
      (is (= :west-of-house (get-in snapshot [:room :id]))))))

(deftest test-object-flags
  (testing "can query object flags"
    (let [state (init-test-state)
          ;; Mailbox should have :cont (container) flag
          mailbox-flags (ml/object-flags state :mailbox)]
      (is (set? mailbox-flags))
      (is (contains? mailbox-flags :cont)))))

(deftest test-json-serialization
  (testing "can serialize snapshot to JSON"
    (let [state (init-test-state)
          snapshot (ml/state-snapshot state)
          json-str (ml/snapshot->json snapshot)]
      (is (string? json-str))
      (is (.contains json-str "score"))
      (is (.contains json-str "west-of-house")))))

(deftest test-execute-action-captures-output
  (testing "execute-action captures output from look"
    (let [state (init-test-state)
          result (ml/execute-action state {:verb :look})]
      (is (string? (:message result)))
      ;; Look should produce output describing the room
      (is (not (empty? (:message result))))
      (is (.contains (:message result) "House"))))

  (testing "execute-action captures output from inventory"
    (let [state (init-test-state)
          result (ml/execute-action state {:verb :inventory})]
      (is (string? (:message result)))
      ;; Inventory when empty should say something
      (is (not (empty? (:message result)))))))

(deftest test-action-parsing
  (testing "can parse action from JSON"
    (let [action (ml/action<-json "{\"verb\": \"look\"}")]
      (is (= :look (:verb action))))
    (let [action (ml/action<-json "{\"verb\": \"take\", \"direct-object\": \"lamp\"}")]
      (is (= :take (:verb action)))
      (is (= :lamp (:direct-object action))))))

;;; ---------------------------------------------------------------------------
;;; REWARD SHAPING TESTS
;;; ---------------------------------------------------------------------------

(deftest test-initial-session
  (testing "initial session has correct structure"
    (let [session (ml/initial-session)]
      (is (set? (:rooms-visited session)))
      (is (empty? (:rooms-visited session)))
      (is (set? (:messages-seen session)))
      (is (= 0 (:total-moves session)))
      (is (= 0 (:max-score session)))
      (is (number? (:start-time session))))))

(deftest test-session-stats
  (testing "session-stats computes correct values"
    (let [session (-> (ml/initial-session)
                      (update :rooms-visited conj :west-of-house :north-of-house)
                      (update :messages-seen conj 12345 67890 11111)
                      (update :objects-taken conj :lamp :sword)
                      (assoc :max-score 25)
                      (assoc :total-moves 10)
                      (assoc :valid-actions 8)
                      (assoc :invalid-actions 2))
          stats (ml/session-stats session)]
      (is (= 2 (:rooms-discovered stats)))
      (is (= 3 (:unique-messages stats)))
      (is (= 2 (:objects-collected stats)))
      (is (= 25 (:max-score stats)))
      (is (= 10 (:total-moves stats)))
      (is (== 0.8 (:valid-action-rate stats))))))

(deftest test-execute-action-with-rewards
  (testing "execute-action-with-rewards returns reward signals"
    (let [state (init-test-state)
          session (-> (ml/initial-session)
                      (update :rooms-visited conj (:here state)))
          action {:verb :go :direction :north}
          result (ml/execute-action-with-rewards state session action)]
      ;; Should have all expected keys
      (is (contains? result :game-state))
      (is (contains? result :message))
      (is (contains? result :rewards))
      (is (contains? result :composite-reward))
      (is (contains? result :session))
      ;; Rewards should have signal keys
      (let [rewards (:rewards result)]
        (is (contains? rewards :score-delta))
        (is (contains? rewards :novel-room?))
        (is (contains? rewards :novel-message?))
        (is (contains? rewards :valid-action?)))
      ;; Session should be updated
      (is (>= (:total-moves (:session result)) 1)))))

(deftest test-novel-room-detection
  (testing "novel room gives exploration bonus"
    (let [state (init-test-state)
          ;; Session starts with only west-of-house visited
          session (-> (ml/initial-session)
                      (update :rooms-visited conj :west-of-house))
          ;; Move to a new room
          action {:verb :go :direction :north}
          result (ml/execute-action-with-rewards state session action)]
      ;; Should detect novel room if we actually moved
      (when (not= (:here state) (:here (:game-state result)))
        (is (:novel-room? (:rewards result)))
        ;; Novel room should be added to session
        (is (contains? (:rooms-visited (:session result))
                       (:here (:game-state result))))))))

(deftest test-message-novelty-tracking
  (testing "new messages are tracked in session"
    (let [state (init-test-state)
          session (ml/initial-session)
          ;; Look command should produce a message
          action {:verb :look}
          result (ml/execute-action-with-rewards state session action)]
      ;; Should have a message
      (when (seq (:message result))
        ;; Should detect as novel (first time seeing this message)
        (is (:novel-message? (:rewards result)))
        ;; Message hash should be added to session
        (is (pos? (count (:messages-seen (:session result)))))))))

(deftest test-composite-reward-calculation
  (testing "composite reward combines signals correctly"
    (let [state (init-test-state)
          session (ml/initial-session)
          ;; Simple action
          action {:verb :look}
          result (ml/execute-action-with-rewards state session action)]
      ;; Composite reward should be a number
      (is (number? (:composite-reward result)))
      ;; Novel message should contribute positive reward
      (when (:novel-message? (:rewards result))
        (is (pos? (:composite-reward result)))))))

;;; ---------------------------------------------------------------------------
;;; DAEMON INTEGRATION TESTS
;;; ---------------------------------------------------------------------------

(defn init-test-state-with-daemons
  "Create a game state with daemons registered (like real game)."
  []
  (-> (init-test-state)
      (daemon/register-daemon :i-fight combat/combat-daemon :tick -1)
      (daemon/register-daemon :i-sword sword/i-sword :tick -1)))

(deftest test-daemon-output-captured
  (testing "daemon output is captured in execute-action message"
    ;; Set up a state where sword is in inventory and we're adjacent to troll
    (let [state (-> (init-test-state-with-daemons)
                    (assoc-in [:objects :sword :in] :adventurer)
                    (assoc-in [:objects :sword :tvalue] 0)  ;; No glow yet
                    (assoc :here :cellar))  ;; Adjacent to troll room
          ;; Execute any action (look) to trigger daemon
          result (ml/execute-action state {:verb :look})]
      ;; The sword daemon should have run and updated tvalue
      (is (= 1 (get-in (:game-state result) [:objects :sword :tvalue]))
          "Sword glow level should be 1 (adjacent to troll)")
      ;; The message should contain the sword glow text
      (is (re-find #"faint blue glow" (:message result))
          "Daemon message should be captured in output"))))
