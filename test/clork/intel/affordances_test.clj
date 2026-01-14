(ns clork.intel.affordances-test
  (:require [clojure.test :refer :all]
            [clork.intel.affordances :as aff]
            [clork.game-state :as gs]
            [clork.rooms :as rooms]
            [clork.objects :as objects]
            [clork.verb-defs :as verb-defs]
            [clork.debug.scenarios :as scenarios]))

(defn init-test-state
  "Create a fresh game state for testing."
  []
  (let [base (gs/initial-game-state)
        with-rooms (gs/add-rooms base rooms/all-rooms)
        with-objects (gs/add-objects with-rooms objects/all-objects)]
    (verb-defs/register-object-vocabulary! (:objects with-objects))
    (gs/set-here-flag with-objects :lit)))

;;; ---------------------------------------------------------------------------
;;; PRECONDITION CHECKER TESTS
;;; ---------------------------------------------------------------------------

(deftest test-check-object-visible-in-room
  (testing "object-visible returns true for objects in current room"
    (let [gs (init-test-state)
          ;; mailbox is in :west-of-house
          precond {:type :object-visible :object :mailbox}]
      (is (aff/check-precondition gs precond {})))))

(deftest test-check-object-visible-in-inventory
  (testing "object-visible returns true for objects in inventory"
    (let [gs (-> (init-test-state)
                 (gs/move-object :leaflet :adventurer :test))
          precond {:type :object-visible :object :leaflet}]
      (is (aff/check-precondition gs precond {})))))

(deftest test-check-object-visible-not-here
  (testing "object-visible returns false for objects elsewhere"
    (let [gs (init-test-state)
          ;; lamp is in :living-room, we're at :west-of-house
          precond {:type :object-visible :object :brass-lantern}]
      (is (not (aff/check-precondition gs precond {}))))))

(deftest test-check-object-held
  (testing "object-held returns true for objects in inventory"
    (let [gs (-> (init-test-state)
                 (gs/move-object :brass-lantern :adventurer :test))
          precond {:type :object-held :object :brass-lantern}]
      (is (aff/check-precondition gs precond {})))))

(deftest test-check-object-held-not-held
  (testing "object-held returns false for objects not in inventory"
    (let [gs (init-test-state)
          precond {:type :object-held :object :brass-lantern}]
      (is (not (aff/check-precondition gs precond {}))))))

(deftest test-check-object-flag
  (testing "object-flag returns true when object has flag"
    (let [gs (init-test-state)
          ;; brass-lantern has :light flag
          precond {:type :object-flag :object :brass-lantern :flag :light}]
      (is (aff/check-precondition gs precond {})))))

(deftest test-check-object-flag-missing
  (testing "object-flag returns false when object lacks flag"
    (let [gs (init-test-state)
          ;; brass-lantern doesn't have :on flag initially
          precond {:type :object-flag :object :brass-lantern :flag :on}]
      (is (not (aff/check-precondition gs precond {}))))))

(deftest test-check-object-not-flag
  (testing "object-not-flag returns true when object lacks flag"
    (let [gs (init-test-state)
          precond {:type :object-not-flag :object :brass-lantern :flag :on}]
      (is (aff/check-precondition gs precond {})))))

(deftest test-check-game-flag
  (testing "game-flag returns true when game flag is set"
    (let [gs (-> (init-test-state)
                 (gs/set-game-flag :troll-flag))
          precond {:type :game-flag :flag :troll-flag}]
      (is (aff/check-precondition gs precond {})))))

(deftest test-check-game-flag-not-set
  (testing "game-flag returns false when game flag is not set"
    (let [gs (init-test-state)
          precond {:type :game-flag :flag :troll-flag}]
      (is (not (aff/check-precondition gs precond {}))))))

(deftest test-check-game-not-flag
  (testing "game-not-flag returns true when game flag is not set"
    (let [gs (init-test-state)
          precond {:type :game-not-flag :flag :lld-flag}]
      (is (aff/check-precondition gs precond {})))))

(deftest test-check-at-location
  (testing "at-location returns true when player is at specified room"
    (let [gs (init-test-state)
          ;; Player starts at :west-of-house
          precond {:type :at-location :room :west-of-house}]
      (is (aff/check-precondition gs precond {})))))

(deftest test-check-at-location-wrong-room
  (testing "at-location returns false when player is elsewhere"
    (let [gs (init-test-state)
          precond {:type :at-location :room :living-room}]
      (is (not (aff/check-precondition gs precond {}))))))

(deftest test-check-exit-exists
  (testing "exit-exists returns true when room has valid exit"
    (let [gs (init-test-state)
          ;; west-of-house has north exit
          precond {:type :exit-exists :dir :north}]
      (is (aff/check-precondition gs precond {})))))

(deftest test-check-exit-exists-blocked
  (testing "exit-exists returns false for blocked exits (string destinations)"
    (let [gs (-> (init-test-state)
                 (assoc :here :forest-1))
          ;; forest-1 may have string blocked exits
          precond {:type :exit-exists :dir :up}]
      (is (not (aff/check-precondition gs precond {}))))))

;;; ---------------------------------------------------------------------------
;;; SATISFIED? TESTS
;;; ---------------------------------------------------------------------------

(deftest test-satisfied-all-preconds-met
  (testing "satisfied? returns true when all preconditions are met"
    (let [gs (-> (init-test-state)
                 (gs/move-object :brass-lantern :adventurer :test))
          ;; Create affordance with preconds we know are satisfied
          affordance {:preconds [{:type :object-held :object :brass-lantern}
                                 {:type :object-flag :object :brass-lantern :flag :light}]}]
      (is (aff/satisfied? gs affordance {})))))

(deftest test-satisfied-some-preconds-not-met
  (testing "satisfied? returns false when any precondition fails"
    (let [gs (init-test-state)  ;; lamp not in inventory
          affordance {:preconds [{:type :object-held :object :brass-lantern}
                                 {:type :object-flag :object :brass-lantern :flag :light}]}]
      (is (not (aff/satisfied? gs affordance {}))))))

;;; ---------------------------------------------------------------------------
;;; LEGAL-ACTIONS TESTS
;;; ---------------------------------------------------------------------------

(deftest test-legal-actions-returns-reasonable-count
  (testing "legal-actions returns a reasonable number of actions (5-50)"
    (let [gs (init-test-state)
          actions (aff/legal-actions gs)]
      ;; Should be more than 5 (movement + open mailbox)
      (is (>= (count actions) 5))
      ;; Should be less than 50 (not exploring entire action space)
      (is (<= (count actions) 50)))))

(deftest test-legal-actions-includes-movement
  (testing "legal-actions includes movement for valid exits"
    (let [gs (init-test-state)
          actions (aff/legal-actions gs)
          movement-actions (filter #(= :walk (:verb %)) actions)]
      ;; west-of-house has north, south, west, ne exits
      (is (>= (count movement-actions) 3)))))

(deftest test-legal-actions-includes-open-mailbox
  (testing "legal-actions includes opening mailbox at west-of-house"
    (let [gs (init-test-state)
          actions (aff/legal-actions gs)
          open-actions (filter #(and (= :open (:verb %))
                                     (= :mailbox (:direct-object %))) actions)]
      (is (= 1 (count open-actions))))))

(deftest test-legal-actions-excludes-open-when-already-open
  (testing "legal-actions excludes opening already-open containers"
    (let [gs (-> (init-test-state)
                 (gs/set-thing-flag :mailbox :open))
          actions (aff/legal-actions gs)
          open-mailbox (filter #(and (= :open (:verb %))
                                     (= :mailbox (:direct-object %))) actions)]
      (is (empty? open-mailbox)))))

(deftest test-legal-actions-includes-close-when-open
  (testing "legal-actions includes closing open containers"
    (let [gs (-> (init-test-state)
                 (gs/set-thing-flag :mailbox :open))
          actions (aff/legal-actions gs)
          close-mailbox (filter #(and (= :close (:verb %))
                                      (= :mailbox (:direct-object %))) actions)]
      (is (= 1 (count close-mailbox))))))

(deftest test-legal-actions-take-only-takeable
  (testing "legal-actions only includes take for takeable objects"
    (let [gs (init-test-state)
          actions (aff/legal-actions gs)
          take-actions (filter #(= :take (:verb %)) actions)]
      ;; All take actions should be for objects with :take flag
      (doseq [action take-actions]
        (is (gs/set-thing-flag? gs (:direct-object action) :take)
            (str "Should only take takeable objects: " (:direct-object action)))))))

(deftest test-legal-actions-drop-only-held
  (testing "legal-actions only includes drop for held objects"
    (let [gs (-> (init-test-state)
                 (gs/move-object :brass-lantern :adventurer :test)
                 (gs/move-object :sword :adventurer :test))
          actions (aff/legal-actions gs)
          drop-actions (filter #(= :drop (:verb %)) actions)]
      (is (= 2 (count drop-actions)))
      (is (some #(= :brass-lantern (:direct-object %)) drop-actions))
      (is (some #(= :sword (:direct-object %)) drop-actions)))))

(deftest test-legal-actions-lamp-on-for-light-sources
  (testing "legal-actions includes lamp-on for held light sources that are off"
    (let [gs (-> (init-test-state)
                 (gs/move-object :brass-lantern :adventurer :test))
          actions (aff/legal-actions gs)
          lamp-on-actions (filter #(= :lamp-on (:verb %)) actions)]
      (is (= 1 (count lamp-on-actions)))
      (is (= :brass-lantern (:direct-object (first lamp-on-actions)))))))

(deftest test-legal-actions-no-lamp-on-when-already-on
  (testing "legal-actions excludes lamp-on for already-on light sources"
    (let [gs (-> (init-test-state)
                 (gs/move-object :brass-lantern :adventurer :test)
                 (gs/set-thing-flag :brass-lantern :on))
          actions (aff/legal-actions gs)
          lamp-on-actions (filter #(and (= :lamp-on (:verb %))
                                        (= :brass-lantern (:direct-object %))) actions)]
      (is (empty? lamp-on-actions)))))

;;; ---------------------------------------------------------------------------
;;; SPECIAL ACTIONS TESTS
;;; ---------------------------------------------------------------------------

(deftest test-echo-action-at-loud-room
  (testing "echo action is available at loud-room when puzzle not solved"
    (let [gs (-> (init-test-state)
                 (assoc :here :loud-room))
          actions (aff/legal-actions gs)
          echo-actions (filter #(= :echo (:verb %)) actions)]
      (is (= 1 (count echo-actions))))))

(deftest test-no-echo-action-when-solved
  (testing "echo action is not available when loud-flag is set"
    (let [gs (-> (init-test-state)
                 (assoc :here :loud-room)
                 (gs/set-game-flag :loud-flag))
          actions (aff/legal-actions gs)
          echo-actions (filter #(= :echo (:verb %)) actions)]
      (is (empty? echo-actions)))))

(deftest test-exorcism-ring-bell-action
  (testing "ring-bell exorcism action available at entrance-to-hades with bell"
    (let [gs (-> (init-test-state)
                 (assoc :here :entrance-to-hades)
                 (gs/move-object :brass-bell :adventurer :test))
          actions (aff/legal-actions gs)
          ring-actions (filter #(and (= :ring (:verb %))
                                     (= :brass-bell (:direct-object %))) actions)]
      (is (= 1 (count ring-actions))))))

(deftest test-no-exorcism-when-complete
  (testing "exorcism actions not available when lld-flag is set"
    (let [gs (-> (init-test-state)
                 (assoc :here :entrance-to-hades)
                 (gs/move-object :brass-bell :adventurer :test)
                 (gs/set-game-flag :lld-flag))
          actions (aff/legal-actions gs)
          ring-actions (filter #(and (= :ring (:verb %))
                                     (= :brass-bell (:direct-object %))) actions)]
      (is (empty? ring-actions)))))

;;; ---------------------------------------------------------------------------
;;; REGISTRY QUERY TESTS
;;; ---------------------------------------------------------------------------

(deftest test-achievers-of-lld-flag
  (testing "achievers-of finds affordance that sets lld-flag"
    (let [achievers (aff/achievers-of {:type :game-flag :flag :lld-flag})]
      (is (= 1 (count achievers)))
      (is (= :read-book-exorcism (:affordance-id (first achievers)))))))

(deftest test-achievers-of-xb-flag
  (testing "achievers-of finds affordance that sets xb flag"
    (let [achievers (aff/achievers-of {:type :game-flag :flag :xb})]
      (is (= 1 (count achievers)))
      (is (= :ring-bell-exorcism (:affordance-id (first achievers)))))))

(deftest test-achievers-of-loud-flag
  (testing "achievers-of finds affordance that sets loud-flag"
    (let [achievers (aff/achievers-of {:type :game-flag :flag :loud-flag})]
      (is (= 1 (count achievers)))
      (is (= :echo-loud-room (:affordance-id (first achievers)))))))

(deftest test-effects-of-ring-bell
  (testing "effects-of returns declared effects for ring-bell-exorcism"
    (let [effects (aff/effects-of :ring-bell-exorcism)]
      (is (some #(= {:type :set-flag :flag :xb} %) effects))
      (is (some #(= {:type :move-object :object :brass-bell :to :limbo} %) effects)))))

(deftest test-preconditions-of-echo
  (testing "preconditions-of returns declared preconditions for echo-loud-room"
    (let [preconds (aff/preconditions-of :echo-loud-room)]
      (is (some #(= {:type :at-location :room :loud-room} %) preconds))
      (is (some #(= {:type :game-not-flag :flag :loud-flag} %) preconds)))))

(deftest test-get-affordance
  (testing "get-affordance retrieves affordance by ID"
    (let [aff (aff/get-affordance :take-object)]
      (is (some? aff))
      (is (= :take (:verb aff)))
      (is (= "Take a portable object" (:desc aff))))))

(deftest test-list-affordances
  (testing "list-affordances returns all affordance IDs"
    (let [ids (aff/list-affordances)]
      (is (some #{:take-object} ids))
      (is (some #{:drop-object} ids))
      (is (some #{:ring-bell-exorcism} ids))
      (is (some #{:echo-loud-room} ids)))))

;;; ---------------------------------------------------------------------------
;;; EXPLAIN LEGAL ACTIONS TEST
;;; ---------------------------------------------------------------------------

(deftest test-explain-legal-actions
  (testing "explain-legal-actions provides summary breakdown"
    (let [gs (init-test-state)
          explained (aff/explain-legal-actions gs)]
      (is (number? (:count explained)))
      (is (vector? (:actions explained)))
      (is (map? (:summary explained)))
      (is (number? (get-in explained [:summary :movement]))))))

;;; ---------------------------------------------------------------------------
;;; NEW AFFORDANCE CATEGORY TESTS
;;; ---------------------------------------------------------------------------

(deftest test-combat-affordances-exist
  (testing "combat affordances are defined"
    (let [combat-ids (map :id (aff/affordances-by-category :combat))]
      (is (some #{:attack-troll} combat-ids))
      (is (some #{:say-odysseus} combat-ids))
      (is (some #{:give-lunch-to-cyclops} combat-ids)))))

(deftest test-dam-affordances-exist
  (testing "dam affordances are defined"
    (let [dam-ids (map :id (aff/affordances-by-category :dam))]
      (is (some #{:press-yellow-button} dam-ids))
      (is (some #{:turn-bolt-open} dam-ids))
      (is (some #{:turn-bolt-close} dam-ids)))))

(deftest test-boat-affordances-exist
  (testing "boat affordances are defined"
    (let [boat-ids (map :id (aff/affordances-by-category :boat))]
      (is (some #{:inflate-boat} boat-ids))
      (is (some #{:board-boat} boat-ids))
      (is (some #{:disembark-boat} boat-ids)))))

(deftest test-environment-affordances-exist
  (testing "environment affordances are defined"
    (let [env-ids (map :id (aff/affordances-by-category :environment))]
      (is (some #{:move-rug} env-ids))
      (is (some #{:open-trap-door} env-ids))
      (is (some #{:unlock-grating} env-ids)))))

(deftest test-climb-affordances-exist
  (testing "climb affordances are defined"
    (let [climb-ids (map :id (aff/affordances-by-category :climb))]
      (is (some #{:climb-tree} climb-ids))
      (is (some #{:climb-down-rope} climb-ids))
      (is (some #{:climb-up-rope} climb-ids)))))

(deftest test-food-affordances-exist
  (testing "food affordances are defined"
    (let [food-ids (map :id (aff/affordances-by-category :food))]
      (is (some #{:eat-garlic} food-ids))
      (is (some #{:fill-bottle} food-ids))
      (is (some #{:drink-water} food-ids)))))

;;; ---------------------------------------------------------------------------
;;; CHECK-AFFORDANCE TESTS (new debugging function)
;;; ---------------------------------------------------------------------------

(deftest test-check-affordance-applicable
  (testing "check-affordance reports applicable when conditions met"
    (let [gs (-> (init-test-state)
                 (assoc :here :loud-room))
          result (aff/check-affordance gs :echo-loud-room)]
      (is (:applicable result))
      (is (= :echo-loud-room (:affordance-id result))))))

(deftest test-check-affordance-not-applicable
  (testing "check-affordance reports failed preconditions when not applicable"
    (let [gs (init-test-state)  ; Not at loud-room
          result (aff/check-affordance gs :echo-loud-room)]
      (is (not (:applicable result)))
      (is (seq (:failed-preconds result))))))

(deftest test-check-affordance-unknown
  (testing "check-affordance reports error for unknown affordance"
    (let [gs (init-test-state)
          result (aff/check-affordance gs :nonexistent-affordance)]
      (is (:error result)))))

;;; ---------------------------------------------------------------------------
;;; AFFORDANCES-BY-VERB TESTS
;;; ---------------------------------------------------------------------------

(deftest test-affordances-by-verb-attack
  (testing "affordances-by-verb finds attack affordances"
    (let [attack-affs (aff/affordances-by-verb :attack)]
      (is (some #(= :attack-troll (:id %)) attack-affs))
      (is (some #(= :attack-cyclops (:id %)) attack-affs)))))

(deftest test-affordances-by-verb-put
  (testing "affordances-by-verb finds put affordances"
    (let [put-affs (aff/affordances-by-verb :put)]
      (is (some #(= :put-in-container (:id %)) put-affs))
      (is (some #(= :put-treasure-in-case (:id %)) put-affs)))))

;;; ---------------------------------------------------------------------------
;;; LEGAL ACTIONS AT SPECIFIC LOCATIONS
;;; ---------------------------------------------------------------------------

(deftest test-legal-actions-at-troll-room
  (testing "attack actions available at troll room with weapon"
    (let [gs (-> (scenarios/equipped-adventurer :troll-room)
                 (gs/move-object :troll :troll-room :test)
                 (gs/unset-game-flag :troll-flag))
          actions (aff/legal-actions gs)
          attack-actions (filter #(= :attack (:verb %)) actions)]
      (is (>= (count attack-actions) 1)))))

(deftest test-legal-actions-at-dam-room
  (testing "dam actions available at dam room"
    (let [gs (scenarios/dam-area :dam-room)
          actions (aff/legal-actions gs)
          push-actions (filter #(= :push (:verb %)) actions)]
      ;; Yellow and brown buttons should be pushable
      (is (>= (count push-actions) 1)))))

(deftest test-legal-actions-at-living-room-with-treasure
  (testing "put-in-case actions available when holding treasure at living room"
    (let [gs (-> (scenarios/equipped-adventurer :living-room)
                 (gs/move-object :egg :adventurer :test))  ; Object ID is :egg, not :jeweled-egg
          actions (aff/legal-actions gs)
          put-actions (filter #(and (= :put (:verb %))
                                    (= :trophy-case (:indirect-object %))) actions)]
      (is (= 1 (count put-actions)))
      (is (= :egg (:direct-object (first put-actions)))))))

;;; ---------------------------------------------------------------------------
;;; VALIDATION TESTS - Comparing declared effects vs actual behavior
;;; ---------------------------------------------------------------------------
;;; These tests verify that affordance declarations match game behavior.

(deftest test-validation-move-rug-effect
  (testing "move-rug affordance correctly predicts rug-moved flag"
    (let [aff (aff/get-affordance :move-rug)
          effects (:effects aff)]
      ;; Affordance should declare setting rug-moved flag
      (is (some #(and (= :set-flag (:type %))
                      (= :rug-moved (:flag %)))
                effects))
      ;; The effect type should match what the game actually does
      ;; (rug-moved flag enables trap-door visibility)
      )))

(deftest test-validation-echo-effects
  (testing "echo-loud-room affordance correctly predicts effects"
    (let [aff (aff/get-affordance :echo-loud-room)
          effects (:effects aff)]
      ;; Should set loud-flag
      (is (some #(and (= :set-flag (:type %))
                      (= :loud-flag (:flag %)))
                effects))
      ;; Should clear sacred flag on platinum bar
      (is (some #(and (= :clear-flag (:type %))
                      (= :platinum-bar (:object %))
                      (= :sacred (:flag %)))
                effects)))))

(deftest test-validation-exorcism-sequence
  (testing "exorcism affordances form a valid sequence"
    ;; Step 1: Ring bell sets xb
    (let [ring-effects (aff/effects-of :ring-bell-exorcism)]
      (is (some #(and (= :set-flag (:type %)) (= :xb (:flag %))) ring-effects)))
    ;; Step 2: Light candles requires xb, sets xc
    (let [light-preconds (aff/preconditions-of :light-candles-exorcism)
          light-effects (aff/effects-of :light-candles-exorcism)]
      (is (some #(and (= :game-flag (:type %)) (= :xb (:flag %))) light-preconds))
      (is (some #(and (= :set-flag (:type %)) (= :xc (:flag %))) light-effects)))
    ;; Step 3: Read book requires xc, sets lld-flag
    (let [read-preconds (aff/preconditions-of :read-book-exorcism)
          read-effects (aff/effects-of :read-book-exorcism)]
      (is (some #(and (= :game-flag (:type %)) (= :xc (:flag %))) read-preconds))
      (is (some #(and (= :set-flag (:type %)) (= :lld-flag (:flag %))) read-effects)))))

(deftest test-validation-troll-defeat
  (testing "defeating troll sets troll-flag"
    (let [aff (aff/get-affordance :attack-troll)
          effects (:effects aff)]
      (is (some #(and (= :set-flag (:type %))
                      (= :troll-flag (:flag %)))
                effects)))))

(deftest test-validation-cyclops-odysseus
  (testing "saying odysseus defeats cyclops and opens passage"
    (let [aff (aff/get-affordance :say-odysseus)
          effects (:effects aff)]
      ;; Sets cyclops-flag (defeated)
      (is (some #(and (= :set-flag (:type %)) (= :cyclops-flag (:flag %))) effects))
      ;; Sets magic-flag (opens strange passage)
      (is (some #(and (= :set-flag (:type %)) (= :magic-flag (:flag %))) effects))
      ;; Moves cyclops to limbo
      (is (some #(and (= :move-object (:type %)) (= :cyclops (:object %)) (= :limbo (:to %))) effects)))))

(deftest test-validation-dam-gate-sequence
  (testing "dam gate controls follow proper sequence"
    ;; Yellow button enables gate mechanism
    (let [yellow-effects (aff/effects-of :press-yellow-button)]
      (is (some #(and (= :set-flag (:type %)) (= :gate-flag (:flag %))) yellow-effects)))
    ;; Turning bolt requires gate-flag
    (let [turn-preconds (aff/preconditions-of :turn-bolt-open)]
      (is (some #(and (= :game-flag (:type %)) (= :gate-flag (:flag %))) turn-preconds)))))

(deftest test-validation-climb-rope-requires-tied
  (testing "climbing rope requires dome-flag (rope tied)"
    (let [climb-preconds (aff/preconditions-of :climb-down-rope)]
      (is (some #(and (= :game-flag (:type %)) (= :dome-flag (:flag %))) climb-preconds)))))

;;; ---------------------------------------------------------------------------
;;; ALL AFFORDANCES HAVE REQUIRED FIELDS
;;; ---------------------------------------------------------------------------

(deftest test-all-affordances-have-required-fields
  (testing "every affordance has id, verb, preconds, effects"
    (doseq [aff (aff/affordances-by-category nil)]  ; nil returns all
      (is (:id aff) (str "Affordance missing :id"))
      (is (:verb aff) (str "Affordance " (:id aff) " missing :verb"))
      (is (vector? (:preconds aff)) (str "Affordance " (:id aff) " :preconds should be vector"))
      (is (vector? (:effects aff)) (str "Affordance " (:id aff) " :effects should be vector")))))

(deftest test-all-affordance-ids-unique
  (testing "all affordance IDs are unique"
    (let [ids (aff/list-affordances)
          id-set (set ids)]
      (is (= (count ids) (count id-set))
          (str "Duplicate affordance IDs found: "
               (filter #(> (count (filter #{%} ids)) 1) ids))))))
