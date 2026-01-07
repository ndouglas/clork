(ns clork.rainbow-test
  "Tests for rainbow actions: crossing when solid, looking under."
  (:require [clojure.test :refer [deftest is testing]]
            [clork.game-state :as gs]
            [clork.core :as core]
            [clork.parser :as parser]
            [clork.verb-defs :as verb-defs]
            [clork.daemon :as daemon]))

;;; ---------------------------------------------------------------------------
;;; Test Utilities
;;; ---------------------------------------------------------------------------

(defn make-test-state
  "Create a test game state with rooms and objects initialized."
  []
  (core/init-game))

(defn run-command
  "Parse and execute a command, returning [output new-state]."
  [game-state input]
  (let [output (java.io.StringWriter.)
        gs (binding [*out* output]
             (-> game-state
                 (assoc :input input)
                 parser/parser-from-input
                 verb-defs/perform
                 daemon/clocker))]
    [(str output) gs]))

;;; ---------------------------------------------------------------------------
;;; Basic Rainbow State Tests
;;; ---------------------------------------------------------------------------

(deftest rainbow-flag-starts-false-test
  (testing "rainbow-flag starts false (rainbow not solid)"
    (let [gs (make-test-state)]
      (is (not (get gs :rainbow-flag false))))))

;;; ---------------------------------------------------------------------------
;;; Cross Rainbow Tests
;;; ---------------------------------------------------------------------------

(deftest cross-rainbow-not-solid-test
  (testing "crossing rainbow when not solid fails"
    (let [gs (-> (make-test-state)
                 (assoc :here :aragain-falls)
                 (assoc :rainbow-flag false)
                 (assoc-in [:objects :rainbow :in] :aragain-falls))
          [output gs'] (run-command gs "cross rainbow")]
      (is (clojure.string/includes? output "water vapor"))
      (is (= :aragain-falls (:here gs'))))))

(deftest cross-rainbow-from-aragain-falls-test
  (testing "crossing solid rainbow from aragain-falls moves to end-of-rainbow"
    (let [gs (-> (make-test-state)
                 (assoc :here :aragain-falls)
                 (assoc :rainbow-flag true)
                 (assoc-in [:objects :rainbow :in] :aragain-falls))
          [output gs'] (run-command gs "cross rainbow")]
      ;; ZIL: GOTO shows room name, no "walk across" message
      (is (clojure.string/includes? output "End of Rainbow"))
      (is (= :end-of-rainbow (:here gs'))))))

(deftest cross-rainbow-from-end-of-rainbow-test
  (testing "crossing solid rainbow from end-of-rainbow moves to aragain-falls"
    (let [gs (-> (make-test-state)
                 (assoc :here :end-of-rainbow)
                 (assoc :rainbow-flag true)
                 (assoc-in [:objects :rainbow :in] :end-of-rainbow))
          [output gs'] (run-command gs "cross rainbow")]
      ;; ZIL: GOTO shows room name, no "walk across" message
      (is (clojure.string/includes? output "Aragain Falls"))
      (is (= :aragain-falls (:here gs'))))))

(deftest cross-rainbow-from-canyon-view-test
  (testing "crossing rainbow from canyon-view gives 'From here?!?' message"
    (let [gs (-> (make-test-state)
                 (assoc :here :canyon-view)
                 (assoc :rainbow-flag true)
                 (assoc-in [:objects :rainbow :in] :canyon-view))
          [output gs'] (run-command gs "cross rainbow")]
      (is (clojure.string/includes? output "From here"))
      (is (= :canyon-view (:here gs'))))))

;;; ---------------------------------------------------------------------------
;;; Look Under Rainbow Tests
;;; ---------------------------------------------------------------------------

(deftest look-under-rainbow-test
  (testing "looking under rainbow shows Frigid River message"
    (let [gs (-> (make-test-state)
                 (assoc :here :aragain-falls)
                 (assoc-in [:objects :rainbow :in] :aragain-falls))
          [output _] (run-command gs "look under rainbow")]
      (is (clojure.string/includes? output "Frigid River")))))

;;; ---------------------------------------------------------------------------
;;; Rainbow Solidification Tests (via Sceptre)
;;; ---------------------------------------------------------------------------

(deftest wave-sceptre-at-rainbow-makes-solid-test
  (testing "waving sceptre at rainbow sets rainbow-flag"
    (let [gs (-> (make-test-state)
                 (assoc :here :aragain-falls)
                 (assoc-in [:objects :sceptre :in] :adventurer))
          [output gs'] (run-command gs "wave sceptre")]
      ;; Should see description of rainbow solidifying
      (is (get gs' :rainbow-flag false)))))

(deftest wave-sceptre-again-toggles-rainbow-off-test
  (testing "waving sceptre again at solid rainbow toggles it off (ZIL behavior)"
    ;; ZIL: SCEPTRE-FUNCTION toggles rainbow - if already solid, it becomes run-of-the-mill
    (let [gs (-> (make-test-state)
                 (assoc :here :aragain-falls)
                 (assoc :rainbow-flag true)
                 (assoc-in [:objects :sceptre :in] :adventurer))
          [output gs'] (run-command gs "wave sceptre")]
      ;; Rainbow becomes non-solid, get "run-of-the-mill" message
      (is (clojure.string/includes? output "run-of-the-mill"))
      (is (not (get gs' :rainbow-flag false))))))
