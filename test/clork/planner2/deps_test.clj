(ns clork.planner2.deps-test
  "Tests for the dependency graph module."
  (:require [clojure.test :refer :all]
            [clork.planner2.deps :as deps]
            [clork.planner2.prep :as prep]))

;;; ---------------------------------------------------------------------------
;;; PREP DEPENDENCY TESTS
;;; ---------------------------------------------------------------------------

(deftest prep-depends-on-prep-test
  (testing "troll-flag has no prep dependencies (only items)"
    (is (empty? (deps/prep-depends-on-prep :troll-flag))))

  (testing "cyclops-flag depends on troll-flag"
    (is (contains? (deps/prep-depends-on-prep :cyclops-flag) :troll-flag)))

  (testing "dome-flag depends on troll-flag"
    (is (contains? (deps/prep-depends-on-prep :dome-flag) :troll-flag)))

  (testing "magic-flag depends on cyclops-flag"
    (is (contains? (deps/prep-depends-on-prep :magic-flag) :cyclops-flag)))

  (testing "lld-flag depends on bell-rang"
    ;; lld-flag requires candles-lit which requires bell-rang
    (let [deps (deps/prep-depends-on-prep :lld-flag)]
      (is (contains? deps :candles-lit)))))

;;; ---------------------------------------------------------------------------
;;; DEPENDENCY GRAPH TESTS
;;; ---------------------------------------------------------------------------

(deftest build-prep-dependency-graph-test
  (testing "Graph is built and contains expected preps"
    (let [graph @deps/prep-dependency-graph]
      (is (map? graph))
      (is (contains? graph :troll-flag))
      (is (contains? graph :cyclops-flag))
      (is (contains? graph :dome-flag)))))

(deftest valid-dependency-graph-test
  (testing "Dependency graph has no cycles"
    (is (deps/valid-dependency-graph?))))

(deftest find-cycle-test
  (testing "No cycle found in valid graph"
    (is (nil? (deps/find-cycle)))))

;;; ---------------------------------------------------------------------------
;;; TRANSITIVE DEPENDENCY TESTS
;;; ---------------------------------------------------------------------------

(deftest transitive-prep-deps-test
  (testing "troll-flag has no transitive deps"
    (let [deps (deps/transitive-prep-deps :troll-flag)]
      (is (= [:troll-flag] deps))))

  (testing "cyclops-flag includes troll-flag"
    (let [deps (deps/transitive-prep-deps :cyclops-flag)]
      (is (some #{:troll-flag} deps))
      (is (some #{:cyclops-flag} deps))))

  (testing "magic-flag includes cyclops-flag and troll-flag"
    (let [deps (deps/transitive-prep-deps :magic-flag)]
      (is (some #{:troll-flag} deps))
      (is (some #{:cyclops-flag} deps))
      (is (some #{:magic-flag} deps)))))

;;; ---------------------------------------------------------------------------
;;; TREASURE DEPENDENCY TESTS
;;; ---------------------------------------------------------------------------

(deftest treasure-requires-preps-test
  (testing "egg requires no preps"
    (is (empty? (deps/treasure-requires-preps :egg))))

  (testing "painting requires troll-flag"
    (is (contains? (deps/treasure-requires-preps :painting) :troll-flag)))

  (testing "ivory-torch requires dome-flag"
    (is (contains? (deps/treasure-requires-preps :ivory-torch) :dome-flag)))

  (testing "crystal-skull requires lld-flag"
    (is (contains? (deps/treasure-requires-preps :crystal-skull) :lld-flag))))

(deftest preps-for-treasure-test
  (testing "egg needs no preps"
    (is (empty? (deps/preps-for-treasure :egg))))

  (testing "painting needs troll-flag"
    (let [preps (deps/preps-for-treasure :painting)]
      (is (some #{:troll-flag} preps))))

  (testing "ivory-torch needs troll-flag and dome-flag in order"
    (let [preps (deps/preps-for-treasure :ivory-torch)]
      (is (some #{:troll-flag} preps))
      (is (some #{:dome-flag} preps))
      ;; troll-flag must come before dome-flag
      (let [troll-idx (.indexOf preps :troll-flag)
            dome-idx (.indexOf preps :dome-flag)]
        (is (< troll-idx dome-idx))))))

;;; ---------------------------------------------------------------------------
;;; TOPOLOGICAL SORT TESTS
;;; ---------------------------------------------------------------------------

(deftest topological-sort-test
  (testing "Empty input returns empty"
    (is (= [] (deps/topological-sort []))))

  (testing "Single prep returns itself"
    (is (= [:troll-flag] (deps/topological-sort [:troll-flag]))))

  (testing "Dependencies come before dependents"
    (let [sorted (deps/topological-sort [:cyclops-flag :troll-flag])]
      (is (= 2 (count sorted)))
      (let [troll-idx (.indexOf sorted :troll-flag)
            cyclops-idx (.indexOf sorted :cyclops-flag)]
        (is (< troll-idx cyclops-idx)))))

  (testing "Complex chain is sorted correctly"
    (let [sorted (deps/topological-sort [:magic-flag :cyclops-flag :troll-flag])]
      (is (= 3 (count sorted)))
      (let [troll-idx (.indexOf sorted :troll-flag)
            cyclops-idx (.indexOf sorted :cyclops-flag)
            magic-idx (.indexOf sorted :magic-flag)]
        (is (< troll-idx cyclops-idx))
        (is (< cyclops-idx magic-idx))))))

;;; ---------------------------------------------------------------------------
;;; PREP LEVELS TESTS
;;; ---------------------------------------------------------------------------

(deftest independent-preps-test
  (testing "Independent preps have no prep dependencies"
    (let [independent (set (deps/independent-preps))]
      ;; troll-flag only needs items, not other preps
      (is (contains? independent :troll-flag))
      ;; rug-moved has no requirements
      (is (contains? independent :rug-moved))
      ;; cyclops-flag depends on troll-flag, so NOT independent
      (is (not (contains? independent :cyclops-flag))))))

(deftest prep-levels-test
  (testing "Prep levels are computed"
    (let [levels (deps/prep-levels)]
      (is (vector? levels))
      (is (pos? (count levels)))
      ;; Level 0 should contain independent preps
      (let [level-0 (set (first levels))]
        (is (contains? level-0 :troll-flag))))))

;;; ---------------------------------------------------------------------------
;;; ANALYSIS TESTS
;;; ---------------------------------------------------------------------------

(deftest analyze-treasure-test
  (testing "Analyze egg (simple treasure)"
    (let [analysis (deps/analyze-treasure :egg)]
      (is (= :egg (:treasure analysis)))
      (is (zero? (:prep-count analysis)))
      (is (not (:has-combat analysis)))))

  (testing "Analyze painting (needs troll)"
    (let [analysis (deps/analyze-treasure :painting)]
      (is (= :painting (:treasure analysis)))
      (is (pos? (:prep-count analysis)))
      (is (:has-combat analysis))))  ; troll-flag requires combat

  (testing "Analyze crystal-skull (complex)"
    (let [analysis (deps/analyze-treasure :crystal-skull)]
      (is (pos? (:prep-count analysis))))))

(deftest complexity-ranking-test
  (testing "Complexity ranking returns all treasures"
    (let [ranking (deps/complexity-ranking)]
      (is (seq ranking))
      ;; egg should be among the simplest
      (let [egg-rank (first (filter #(= :egg (:treasure %)) ranking))]
        (is (some? egg-rank))
        (is (zero? (:prep-count egg-rank)))))))
