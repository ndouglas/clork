(ns clork.planner2.verify-test
  "Tests for post-action verification module."
  (:require [clojure.test :refer :all]
            [clork.planner2.verify :as verify]
            [clork.core :as core]
            [clork.game-state :as gs]))

;;; ---------------------------------------------------------------------------
;;; FIXTURES
;;; ---------------------------------------------------------------------------

(defn fresh-game []
  (core/init-game))

;;; ---------------------------------------------------------------------------
;;; RESULT CONSTRUCTION TESTS
;;; ---------------------------------------------------------------------------

(deftest make-result-test
  (testing "Success result"
    (let [result (verify/success {:type :move :to :kitchen} :kitchen)]
      (is (:success? result))
      (is (= :kitchen (:expected result)))
      (is (= :kitchen (:actual result)))
      (is (empty? (:side-effects result)))))

  (testing "Failure result"
    (let [result (verify/failure {:type :move :to :kitchen} :kitchen :living-room)]
      (is (not (:success? result)))
      (is (= :kitchen (:expected result)))
      (is (= :living-room (:actual result))))))

(deftest with-side-effects-test
  (testing "Adding side effects"
    (let [result (verify/success {:type :take :item :egg} :egg)
          with-se (verify/with-side-effects result [{:type :theft :items [:sword]}])]
      (is (= 1 (count (:side-effects with-se))))
      (is (= :theft (:type (first (:side-effects with-se))))))))

;;; ---------------------------------------------------------------------------
;;; MOVE VERIFICATION TESTS
;;; ---------------------------------------------------------------------------

(deftest verify-move-test
  (testing "Successful move"
    (let [pre (fresh-game)
          post (assoc pre :here :living-room)
          result (verify/verify-move pre post :living-room)]
      (is (:success? result))))

  (testing "Failed move"
    (let [pre (fresh-game)
          post pre  ; Didn't move
          result (verify/verify-move pre post :living-room)]
      (is (not (:success? result)))
      (is (= :living-room (:expected result))))))

;;; ---------------------------------------------------------------------------
;;; TAKE VERIFICATION TESTS
;;; ---------------------------------------------------------------------------

(deftest verify-take-test
  (testing "Successful take"
    (let [pre (fresh-game)
          post (assoc-in pre [:objects :sword :in] :adventurer)
          result (verify/verify-take pre post :sword)]
      (is (:success? result))))

  (testing "Failed take"
    (let [pre (fresh-game)
          post pre  ; Item not taken
          result (verify/verify-take pre post :sword)]
      (is (not (:success? result))))))

;;; ---------------------------------------------------------------------------
;;; OPEN/CLOSE VERIFICATION TESTS
;;; ---------------------------------------------------------------------------

(deftest verify-open-test
  (testing "Successful open"
    (let [pre (fresh-game)
          post (gs/set-flag pre :objects :trophy-case :open)
          result (verify/verify-open pre post :trophy-case)]
      (is (:success? result))))

  (testing "Failed open"
    (let [pre (fresh-game)
          post pre
          result (verify/verify-open pre post :trophy-case)]
      (is (not (:success? result))))))

;;; ---------------------------------------------------------------------------
;;; TURN ON/OFF VERIFICATION TESTS
;;; ---------------------------------------------------------------------------

(deftest verify-turn-on-test
  (testing "Successful turn on"
    (let [pre (fresh-game)
          post (gs/set-flag pre :objects :brass-lantern :on)
          result (verify/verify-turn-on pre post :brass-lantern)]
      (is (:success? result))))

  (testing "Failed turn on"
    (let [pre (fresh-game)
          post pre
          result (verify/verify-turn-on pre post :brass-lantern)]
      (is (not (:success? result))))))

;;; ---------------------------------------------------------------------------
;;; COMBAT VERIFICATION TESTS
;;; ---------------------------------------------------------------------------

(deftest verify-combat-test
  (testing "Successful combat - enemy dead"
    (let [pre (fresh-game)
          post (-> pre
                   (assoc-in [:objects :troll :in] :limbo))
          result (verify/verify-combat pre post :troll)]
      (is (:success? result))))

  (testing "Failed combat - enemy alive"
    (let [pre (fresh-game)
          post pre
          result (verify/verify-combat pre post :troll)]
      (is (not (:success? result))))))

;;; ---------------------------------------------------------------------------
;;; GENERIC VERIFICATION TESTS
;;; ---------------------------------------------------------------------------

(deftest verify-action-test
  (testing "Dispatch to correct verifier"
    (let [pre (fresh-game)
          post (assoc pre :here :kitchen)
          result (verify/verify-action pre post {:type :move :to :kitchen})]
      (is (:success? result))))

  (testing "Unknown action type succeeds by default"
    (let [pre (fresh-game)
          result (verify/verify-action pre pre {:type :unknown-action})]
      (is (:success? result)))))

;;; ---------------------------------------------------------------------------
;;; SIDE EFFECT DETECTION TESTS
;;; ---------------------------------------------------------------------------

(deftest detect-side-effects-test
  (testing "Theft detected as side effect"
    (let [pre (-> (fresh-game)
                  (assoc-in [:objects :egg :in] :adventurer))
          post (assoc-in pre [:objects :egg :in] :thief)
          effects (verify/detect-side-effects pre post)]
      (is (some #(= :theft (:type %)) effects))))

  (testing "No side effects when nothing changed"
    (let [gs (fresh-game)
          effects (verify/detect-side-effects gs gs)]
      ;; Should be empty or minimal
      (is (< (count effects) 3)))))

;;; ---------------------------------------------------------------------------
;;; BATCH VERIFICATION TESTS
;;; ---------------------------------------------------------------------------

(deftest all-succeeded-test
  (testing "All successful"
    (let [results [(verify/success {:type :move} :a)
                   (verify/success {:type :take} :b)]]
      (is (verify/all-succeeded? results))))

  (testing "Some failures"
    (let [results [(verify/success {:type :move} :a)
                   (verify/failure {:type :take} :b :c)]]
      (is (not (verify/all-succeeded? results))))))

(deftest failures-only-test
  (testing "Filter failures"
    (let [results [(verify/success {:type :move} :a)
                   (verify/failure {:type :take} :b :c)
                   (verify/success {:type :open} :d)]]
      (is (= 1 (count (verify/failures-only results)))))))

(deftest summarize-results-test
  (testing "Summary statistics"
    (let [results [(verify/success {:type :move} :a)
                   (verify/failure {:type :take} :b :c)
                   (verify/success {:type :open} :d)]
          summary (verify/summarize-results results)]
      (is (= 3 (:total summary)))
      (is (= 2 (:successes summary)))
      (is (= 1 (:failures summary)))
      (is (> (:success-rate summary) 0.6)))))

;;; ---------------------------------------------------------------------------
;;; FORMAT OUTPUT TESTS
;;; ---------------------------------------------------------------------------

(deftest format-result-test
  (testing "Success format"
    (let [result (verify/success {:type :move :to :kitchen} :kitchen)
          formatted (verify/format-result result)]
      (is (clojure.string/includes? formatted "[OK]"))))

  (testing "Failure format"
    (let [result (verify/failure {:type :move :to :kitchen} :kitchen :cellar)
          formatted (verify/format-result result)]
      (is (clojure.string/includes? formatted "[FAIL]"))
      (is (clojure.string/includes? formatted "Expected")))))

