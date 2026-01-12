(ns clork.planner2.prerequisites-test
  "Comprehensive tests for puzzle prerequisites for all treasures.

   Verifies that the planner correctly understands:
   - What prep actions are needed to reach each treasure
   - The order in which prep actions must be executed
   - Transitive dependencies are correctly computed
   - Complex puzzle chains (exorcism, dam, etc.) are modeled correctly"
  (:require [clojure.test :refer :all]
            [clork.planner2.deps :as deps]
            [clork.planner2.prep :as prep]
            [clork.planner2.goals :as goals]
            [clojure.set :as set]))

;;; ---------------------------------------------------------------------------
;;; TREASURE COVERAGE TESTS
;;; ---------------------------------------------------------------------------

(deftest all-treasures-have-prerequisites-defined
  (testing "Every treasure in goals.clj has prerequisite requirements defined"
    (doseq [treasure goals/treasures]
      (testing (str "Treasure: " (name treasure))
        ;; Either the treasure has an entry in treasure-prep-requirements
        ;; or it's mapped to a different name (like :trunk for :jewel-encrusted-trunk)
        (let [direct-entry (get prep/treasure-prep-requirements treasure)
              ;; Some treasures map to different keys
              alt-key (case treasure
                        :jewel-encrusted-trunk :trunk
                        nil)
              alt-entry (when alt-key (get prep/treasure-prep-requirements alt-key))
              has-entry (or direct-entry alt-entry)]
          (is (some? has-entry)
              (str treasure " should have prerequisites defined")))))))

;;; ---------------------------------------------------------------------------
;;; SIMPLE TREASURE TESTS (No prerequisites)
;;; ---------------------------------------------------------------------------

(deftest egg-requires-no-preps
  (testing "Egg can be obtained without any prep actions"
    (let [preps (deps/preps-for-treasure :egg)]
      (is (empty? preps))
      (is (empty? (deps/treasure-requires-preps :egg))))))

;;; ---------------------------------------------------------------------------
;;; TROLL-GATED TREASURE TESTS
;;; ---------------------------------------------------------------------------

(deftest troll-gated-treasures
  (testing "Treasures behind the troll require :troll-flag"
    (doseq [treasure [:painting :bag-of-coins :silver-chalice :brass-bauble]]
      (testing (str "Treasure: " (name treasure))
        (let [preps (deps/treasure-requires-preps treasure)]
          (is (contains? preps :troll-flag)
              (str treasure " should require :troll-flag")))))))

;;; ---------------------------------------------------------------------------
;;; DOME/TORCH ROOM TREASURES
;;; ---------------------------------------------------------------------------

(deftest dome-gated-treasures
  (testing "Treasures in torch room area require :dome-flag"
    (doseq [treasure [:ivory-torch :sceptre]]
      (testing (str "Treasure: " (name treasure))
        (let [preps (deps/treasure-requires-preps treasure)]
          (is (contains? preps :dome-flag)
              (str treasure " should require :dome-flag"))
          (is (contains? preps :troll-flag)
              (str treasure " should also require :troll-flag (transitive)")))))))

(deftest dome-flag-ordering
  (testing "Dome flag requires troll flag first in execution order"
    (let [preps (deps/preps-for-treasure :ivory-torch)]
      (when (and (some #{:troll-flag} preps)
                 (some #{:dome-flag} preps))
        (let [troll-idx (.indexOf preps :troll-flag)
              dome-idx (.indexOf preps :dome-flag)]
          (is (< troll-idx dome-idx)
              ":troll-flag must be completed before :dome-flag"))))))

;;; ---------------------------------------------------------------------------
;;; DAM PUZZLE / LOW TIDE TREASURES
;;; ---------------------------------------------------------------------------

(deftest low-tide-treasures
  (testing "Treasures requiring low tide have :low-tide in requirements"
    (doseq [treasure [:jade-figurine :sapphire-bracelet]]
      (testing (str "Treasure: " (name treasure))
        (let [preps (deps/treasure-requires-preps treasure)]
          (is (contains? preps :low-tide)
              (str treasure " should require :low-tide")))))))

(deftest low-tide-dependency-chain
  (testing "Low tide has proper transitive dependencies"
    (let [preps (deps/transitive-prep-deps :low-tide)]
      ;; low-tide requires gates-open which requires gate-flag and wrench
      (is (some #{:gates-open} preps)
          ":low-tide requires :gates-open")
      (is (some #{:gate-flag} preps)
          ":low-tide transitively requires :gate-flag")
      ;; gate-flag requires troll-flag
      (is (some #{:troll-flag} preps)
          ":low-tide transitively requires :troll-flag"))))

(deftest dam-puzzle-ordering
  (testing "Dam puzzle preps are in correct order"
    (let [preps (deps/transitive-prep-deps :low-tide)]
      (when (and (some #{:gate-flag} preps)
                 (some #{:gates-open} preps)
                 (some #{:low-tide} preps))
        (let [indices (fn [p] (.indexOf preps p))
              gate-idx (indices :gate-flag)
              gates-idx (indices :gates-open)
              low-tide-idx (indices :low-tide)]
          (is (< gate-idx gates-idx)
              ":gate-flag before :gates-open")
          (is (< gates-idx low-tide-idx)
              ":gates-open before :low-tide"))))))

;;; ---------------------------------------------------------------------------
;;; LOUD ROOM PUZZLE
;;; ---------------------------------------------------------------------------

(deftest platinum-bar-prerequisites
  (testing "Platinum bar requires loud-flag and troll-flag"
    (let [preps (deps/treasure-requires-preps :platinum-bar)]
      (is (contains? preps :loud-flag)
          "Platinum bar requires :loud-flag")
      (is (contains? preps :troll-flag)
          "Platinum bar requires :troll-flag"))))

(deftest loud-flag-dependency
  (testing "Loud flag depends on troll flag"
    (let [preps (deps/transitive-prep-deps :loud-flag)]
      (is (some #{:troll-flag} preps)
          ":loud-flag requires reaching loud room via troll"))))

;;; ---------------------------------------------------------------------------
;;; RAINBOW PUZZLE
;;; ---------------------------------------------------------------------------

(deftest pot-of-gold-prerequisites
  (testing "Pot of gold requires rainbow-flag"
    (let [preps (deps/treasure-requires-preps :pot-of-gold)]
      (is (contains? preps :rainbow-flag)
          "Pot of gold requires :rainbow-flag"))))

(deftest rainbow-flag-requires-sceptre
  (testing "Rainbow flag requires having the sceptre"
    (let [req (prep/prep-requires :rainbow-flag)]
      (is (contains? req :sceptre)
          "Waving sceptre creates rainbow"))))

;;; ---------------------------------------------------------------------------
;;; EXORCISM PUZZLE (Complex multi-step)
;;; ---------------------------------------------------------------------------

(deftest crystal-skull-prerequisites
  (testing "Crystal skull requires completing exorcism"
    (let [preps (deps/treasure-requires-preps :crystal-skull)]
      (is (contains? preps :lld-flag)
          "Crystal skull requires :lld-flag (exorcism complete)"))))

(deftest exorcism-dependency-chain
  (testing "Exorcism has correct multi-step dependencies"
    (let [preps (deps/transitive-prep-deps :lld-flag)]
      ;; lld-flag requires candles-lit
      (is (some #{:candles-lit} preps)
          ":lld-flag requires :candles-lit")
      ;; candles-lit requires bell-rang
      (is (some #{:bell-rang} preps)
          "Exorcism requires :bell-rang"))))

(deftest exorcism-ordering
  (testing "Exorcism steps must be in correct order"
    (let [preps (deps/transitive-prep-deps :lld-flag)]
      (when (and (some #{:bell-rang} preps)
                 (some #{:candles-lit} preps)
                 (some #{:lld-flag} preps))
        (let [indices (fn [p] (.indexOf preps p))
              bell-idx (indices :bell-rang)
              candles-idx (indices :candles-lit)
              lld-idx (indices :lld-flag)]
          (is (< bell-idx candles-idx)
              ":bell-rang before :candles-lit")
          (is (< candles-idx lld-idx)
              ":candles-lit before :lld-flag"))))))

(deftest exorcism-item-requirements
  (testing "Exorcism requires specific items"
    (let [bell-req (prep/prep-requires :bell-rang)]
      (is (contains? bell-req :bell))
      (is (contains? bell-req :candles))
      (is (contains? bell-req :book)))
    (let [candles-req (prep/prep-requires :candles-lit)]
      (is (contains? candles-req :matches))
      (is (contains? candles-req :candles)))))

;;; ---------------------------------------------------------------------------
;;; CYCLOPS PUZZLE
;;; ---------------------------------------------------------------------------

(deftest cyclops-flag-dependency
  (testing "Cyclops flag requires reaching cyclops room"
    (let [req (prep/prep-requires :cyclops-flag)]
      (is (contains? req :troll-flag)
          "Must kill troll to reach cyclops room"))))

(deftest magic-flag-dependency
  (testing "Magic flag requires cyclops flag"
    (let [deps (deps/prep-depends-on-prep :magic-flag)]
      (is (contains? deps :cyclops-flag)
          ":magic-flag depends on :cyclops-flag"))))

;;; ---------------------------------------------------------------------------
;;; COAL MINE / DIAMOND PUZZLE
;;; ---------------------------------------------------------------------------

(deftest huge-diamond-prerequisites
  (testing "Huge diamond requires machine activation"
    (let [preps (deps/treasure-requires-preps :huge-diamond)]
      (is (contains? preps :machine-activated)
          "Diamond requires :machine-activated"))))

(deftest machine-dependency-chain
  (testing "Machine activation requires coal in machine"
    (let [deps (deps/prep-depends-on-prep :machine-activated)]
      (is (contains? deps :coal-in-machine)
          ":machine-activated depends on :coal-in-machine"))))

(deftest coal-mine-requires-troll
  (testing "Coal mine area requires troll flag"
    (let [req (prep/prep-requires :coal-in-machine)]
      (is (contains? req :troll-flag)
          "Must kill troll to reach coal mine"))))

;;; ---------------------------------------------------------------------------
;;; THIEF-RELATED TREASURES
;;; ---------------------------------------------------------------------------

(deftest clockwork-canary-prerequisites
  (testing "Clockwork canary requires killing thief"
    (let [preps (deps/treasure-requires-preps :clockwork-canary)]
      (is (contains? preps :thief-flag)
          "Canary requires :thief-flag (thief opens egg)"))))

(deftest trunk-prerequisites
  (testing "Trunk requires killing thief and low tide"
    (let [preps (deps/treasure-requires-preps :trunk)]
      (is (contains? preps :thief-flag)
          "Trunk requires :thief-flag")
      (is (contains? preps :low-tide)
          "Trunk requires :low-tide (in thief's lair)"))))

;;; ---------------------------------------------------------------------------
;;; GOLD COFFIN / PRAYER TELEPORT
;;; ---------------------------------------------------------------------------

(deftest gold-coffin-prerequisites
  (testing "Gold coffin requires coffin cure flag"
    (let [preps (deps/treasure-requires-preps :gold-coffin)]
      (is (contains? preps :coffin-cure)
          "Coffin requires :coffin-cure to exit temple"))))

(deftest coffin-cure-requires-dome
  (testing "Coffin cure requires reaching temple via dome"
    (let [req (prep/prep-requires :coffin-cure)]
      (is (contains? req :dome-flag)
          "Temple access requires :dome-flag"))))

;;; ---------------------------------------------------------------------------
;;; DEPENDENCY GRAPH INTEGRITY TESTS
;;; ---------------------------------------------------------------------------

(deftest no-cycles-in-dependencies
  (testing "Dependency graph has no cycles"
    (is (deps/valid-dependency-graph?)
        "Dependency graph should be acyclic")))

(deftest all-preps-have-valid-requirements
  (testing "All prep requirements reference valid items or flags"
    (let [valid-flags (set (keys prep/prep-actions))]
      (doseq [[prep-id info] prep/prep-actions]
        (testing (str "Prep: " (name prep-id))
          (doseq [req (:requires info #{})]
            ;; Requirement should be either a known prep flag or an item
            (when (contains? valid-flags req)
              (is (not (nil? (get prep/prep-actions req)))
                  (str "Requirement " req " should be a valid prep")))))))))

(deftest transitive-deps-include-all-direct-deps
  (testing "Transitive dependencies include all direct dependencies"
    (doseq [prep-id (prep/all-preps)]
      (let [direct (deps/prep-depends-on-prep prep-id)
            transitive (set (deps/transitive-prep-deps prep-id))]
        (doseq [d direct]
          (is (contains? transitive d)
              (str prep-id " transitive deps should include " d)))))))

;;; ---------------------------------------------------------------------------
;;; COMPLEXITY ANALYSIS TESTS
;;; ---------------------------------------------------------------------------

(deftest treasure-complexity-ordering
  (testing "Treasures are rankable by complexity"
    (let [ranking (deps/complexity-ranking)]
      (is (seq ranking) "Should produce a ranking")
      ;; Simple treasures should have low prep count
      (let [simple-treasures #{:egg}
            simple-ranks (filter #(simple-treasures (:treasure %)) ranking)]
        (doseq [r simple-ranks]
          (is (zero? (:prep-count r))
              (str (:treasure r) " should have 0 preps"))))
      ;; Complex treasures should have higher prep count
      (let [complex-treasures #{:crystal-skull :gold-coffin}
            complex-ranks (filter #(complex-treasures (:treasure %)) ranking)]
        (doseq [r complex-ranks]
          (is (> (:prep-count r) 1)
              (str (:treasure r) " should have multiple preps")))))))

(deftest prep-chain-lengths
  (testing "Prep chains have reasonable lengths"
    (doseq [prep-id (prep/all-preps)]
      (let [chain (deps/prep-chain prep-id)]
        (is (seq chain) (str prep-id " should have a non-empty chain"))
        (is (<= (count chain) 10)
            (str prep-id " chain should not be unreasonably long"))))))

;;; ---------------------------------------------------------------------------
;;; TIMING-SENSITIVE PUZZLE TESTS
;;; ---------------------------------------------------------------------------

(deftest dam-is-timed-prep
  (testing "Dam puzzle has timed effects"
    (is (prep/timed-prep? :low-tide)
        ":low-tide is a timed effect")))

(deftest exorcism-time-limits
  (testing "Exorcism has time limits defined"
    (let [candles-info (get prep/prep-actions :candles-lit)]
      (is (some? (:time-limit candles-info))
          "Candles lighting has time limit"))
    (let [lld-info (get prep/prep-actions :lld-flag)]
      (is (some? (:time-limit lld-info))
          "Book reading has time limit"))))

(deftest combat-preps-identified
  (testing "Combat prep actions are correctly identified"
    (is (prep/combat-prep? :troll-flag)
        ":troll-flag requires combat")
    (is (prep/combat-prep? :thief-flag)
        ":thief-flag requires combat")
    (is (not (prep/combat-prep? :rug-moved))
        ":rug-moved doesn't require combat")))

;;; ---------------------------------------------------------------------------
;;; TREASURE LOCATION / ITEM DEPENDENCY TESTS
;;; ---------------------------------------------------------------------------

(deftest items-have-location-info
  (testing "Prep items have location information"
    (doseq [[item info] prep/prep-item-locations]
      (testing (str "Item: " (name item))
        (is (some? (:location info))
            (str item " should have a location"))))))

(deftest item-dependencies-include-troll
  (testing "Items in underground require troll flag"
    (let [underground-items [:wrench :skeleton-key :matches :coal :air-pump
                             :pile-of-plastic :putty]]
      (doseq [item underground-items]
        (when-let [info (get prep/prep-item-locations item)]
          (testing (str "Item: " (name item))
            (when-let [reqs (:requires info)]
              (is (contains? reqs :troll-flag)
                  (str item " should require :troll-flag")))))))))

;;; ---------------------------------------------------------------------------
;;; COMPREHENSIVE TREASURE CHAIN TESTS
;;; ---------------------------------------------------------------------------

(deftest painting-full-chain
  (testing "Painting: kill troll, navigate to gallery"
    (let [preps (deps/preps-for-treasure :painting)]
      (is (some #{:troll-flag} preps)
          "Painting requires killing troll"))))

(deftest crystal-skull-full-chain
  (testing "Crystal skull: troll -> dome -> temple -> exorcism"
    (let [preps (deps/preps-for-treasure :crystal-skull)]
      ;; Should include all steps
      (is (some #{:troll-flag} preps))
      (is (some #{:dome-flag} preps))
      (is (some #{:lld-flag} preps))
      ;; Verify ordering
      (when (and (some #{:troll-flag} preps)
                 (some #{:dome-flag} preps)
                 (some #{:lld-flag} preps))
        (let [idx (fn [p] (.indexOf preps p))]
          (is (< (idx :troll-flag) (idx :dome-flag)))
          (is (< (idx :dome-flag) (idx :lld-flag))))))))

(deftest huge-diamond-full-chain
  (testing "Diamond: troll -> coal mine -> machine"
    (let [preps (deps/preps-for-treasure :huge-diamond)]
      (is (some #{:troll-flag} preps))
      (is (some #{:coal-in-machine} preps))
      (is (some #{:machine-activated} preps))
      (when (every? #(some #{%} preps) [:troll-flag :coal-in-machine :machine-activated])
        (let [idx (fn [p] (.indexOf preps p))]
          (is (< (idx :troll-flag) (idx :coal-in-machine)))
          (is (< (idx :coal-in-machine) (idx :machine-activated))))))))

