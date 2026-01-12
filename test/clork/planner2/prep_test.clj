(ns clork.planner2.prep-test
  "Tests for the prep actions catalog."
  (:require [clojure.test :refer :all]
            [clork.planner2.prep :as prep]))

;;; ---------------------------------------------------------------------------
;;; DATA INTEGRITY TESTS
;;; ---------------------------------------------------------------------------

(deftest prep-actions-structure-test
  (testing "All prep actions have required keys"
    (doseq [[prep-id info] prep/prep-actions]
      (testing (str "Prep: " prep-id)
        (is (contains? info :description) (str prep-id " missing :description"))
        (is (contains? info :requires) (str prep-id " missing :requires"))
        (is (contains? info :action) (str prep-id " missing :action"))
        (is (contains? info :enables) (str prep-id " missing :enables"))))))

(deftest prep-actions-requires-are-sets-test
  (testing "All :requires values are sets"
    (doseq [[prep-id info] prep/prep-actions]
      (is (set? (:requires info))
          (str prep-id " :requires is not a set")))))

(deftest prep-actions-enables-are-sets-test
  (testing "All :enables values are sets"
    (doseq [[prep-id info] prep/prep-actions]
      (is (set? (:enables info))
          (str prep-id " :enables is not a set")))))

;;; ---------------------------------------------------------------------------
;;; HELPER FUNCTION TESTS
;;; ---------------------------------------------------------------------------

(deftest prep-requires-test
  (testing "troll-flag requires sword and lantern"
    (let [reqs (prep/prep-requires :troll-flag)]
      (is (contains? reqs :sword))
      (is (contains? reqs :brass-lantern))))

  (testing "rug-moved has no requirements"
    (is (empty? (prep/prep-requires :rug-moved))))

  (testing "dome-flag requires rope and troll-flag"
    (let [reqs (prep/prep-requires :dome-flag)]
      (is (contains? reqs :rope))
      (is (contains? reqs :troll-flag)))))

(deftest prep-enables-test
  (testing "troll-flag enables many rooms"
    (let [enables (prep/prep-enables :troll-flag)]
      (is (contains? enables :east-west-passage))
      (is (contains? enables :cyclops-room))))

  (testing "dome-flag enables torch-room"
    (is (contains? (prep/prep-enables :dome-flag) :torch-room)))

  (testing "magic-flag enables strange-passage and treasure-room"
    (let [enables (prep/prep-enables :magic-flag)]
      (is (contains? enables :strange-passage))
      (is (contains? enables :treasure-room)))))

(deftest prep-location-test
  (testing "Known prep locations"
    (is (= :troll-room (prep/prep-location :troll-flag)))
    (is (= :cyclops-room (prep/prep-location :cyclops-flag)))
    (is (= :dome-room (prep/prep-location :dome-flag)))
    (is (= :loud-room (prep/prep-location :loud-flag)))
    (is (= :living-room (prep/prep-location :rug-moved)))))

(deftest prep-action-test
  (testing "Combat preps have :combat action"
    (is (= :combat (prep/prep-action :troll-flag)))
    (is (= :combat (prep/prep-action :thief-flag))))

  (testing "Map actions have verb"
    (let [action (prep/prep-action :dome-flag)]
      (is (map? action))
      (is (= :tie (:verb action))))))

(deftest prep-delay-test
  (testing "gates-open has delay"
    (is (= 8 (prep/prep-delay :gates-open))))

  (testing "troll-flag has no delay"
    (is (nil? (prep/prep-delay :troll-flag)))))

(deftest timed-prep-test
  (testing "gates-open is timed"
    (is (prep/timed-prep? :gates-open)))

  (testing "troll-flag is not timed"
    (is (not (prep/timed-prep? :troll-flag)))))

(deftest combat-prep-test
  (testing "troll-flag is combat"
    (is (prep/combat-prep? :troll-flag)))

  (testing "dome-flag is not combat"
    (is (not (prep/combat-prep? :dome-flag)))))

(deftest all-preps-test
  (testing "all-preps returns all prep IDs"
    (let [preps (prep/all-preps)]
      (is (seq preps))
      (is (some #{:troll-flag} preps))
      (is (some #{:cyclops-flag} preps))
      (is (some #{:dome-flag} preps)))))

;;; ---------------------------------------------------------------------------
;;; TREASURE REQUIREMENTS TESTS
;;; ---------------------------------------------------------------------------

(deftest treasure-prep-requirements-test
  (testing "egg has no requirements"
    (is (empty? (get-in prep/treasure-prep-requirements [:egg :required]))))

  (testing "painting requires troll-flag"
    (is (contains? (get-in prep/treasure-prep-requirements [:painting :required])
                   :troll-flag)))

  (testing "ivory-torch requires dome-flag"
    (is (contains? (get-in prep/treasure-prep-requirements [:ivory-torch :required])
                   :dome-flag)))

  (testing "crystal-skull requires lld-flag"
    (is (contains? (get-in prep/treasure-prep-requirements [:crystal-skull :required])
                   :lld-flag))))

;;; ---------------------------------------------------------------------------
;;; QUERY FUNCTION TESTS
;;; ---------------------------------------------------------------------------

(deftest preps-enabling-test
  (testing "Find preps that enable torch-room"
    (let [preps (prep/preps-enabling :torch-room)]
      (is (some #{:dome-flag} preps))))

  (testing "Find preps that enable treasure-room"
    (let [preps (prep/preps-enabling :treasure-room)]
      (is (some #{:magic-flag} preps)))))

(deftest preps-requiring-flag-test
  (testing "Find preps requiring troll-flag"
    (let [preps (prep/preps-requiring-flag :troll-flag)]
      (is (some #{:cyclops-flag} preps))
      (is (some #{:dome-flag} preps)))))

(deftest preps-requiring-item-test
  (testing "Find preps requiring rope"
    (let [preps (prep/preps-requiring-item :rope)]
      (is (some #{:dome-flag} preps))))

  (testing "Find preps requiring sword"
    (let [preps (prep/preps-requiring-item :sword)]
      (is (some #{:troll-flag} preps)))))

;;; ---------------------------------------------------------------------------
;;; TIMED EFFECTS TESTS
;;; ---------------------------------------------------------------------------

(deftest timed-effects-structure-test
  (testing "Timed effects have expected structure"
    (is (map? prep/timed-effects))
    (is (contains? prep/timed-effects :low-tide))
    (is (contains? prep/timed-effects :candles))
    (is (contains? prep/timed-effects :lantern))))

(deftest low-tide-timed-effect-test
  (testing "Low tide effect configuration"
    (let [effect (get prep/timed-effects :low-tide)]
      (is (= :gates-open (:trigger effect)))
      (is (= 8 (:delay effect))))))
