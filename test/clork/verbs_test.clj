(ns clork.verbs-test
  "Verb handler tests for Clork."
  (:require [clojure.test :refer :all]
            [clork.verbs :as verbs]
            [clork.verb-defs :as verb-defs]
            [clork.parser :as parser]
            [clork.game-state :as gs]
            [clork.utils-test :refer [with-captured-output make-test-state parse-test-input]]))

;;; ---------------------------------------------------------------------------
;;; VERB HANDLER TESTS
;;; ---------------------------------------------------------------------------

(deftest v-verbose-test
  (testing "v-verbose sets verbose to true and super-brief to false"
    (let [[output state] (with-captured-output (verbs/v-verbose (gs/initial-game-state)))]
      (is (= true (:verbose state)))
      (is (= false (:super-brief state)))
      (is (= "Maximum verbosity." output)))))

(deftest v-brief-test
  (testing "v-brief sets verbose and super-brief to false"
    (let [[output state] (with-captured-output (verbs/v-brief (gs/initial-game-state)))]
      (is (= false (:verbose state)))
      (is (= false (:super-brief state)))
      (is (= "Brief descriptions." output)))))

(deftest v-super-brief-test
  (testing "v-super-brief sets super-brief to true"
    (let [[output state] (with-captured-output (verbs/v-super-brief (gs/initial-game-state)))]
      (is (= true (:super-brief state)))
      (is (= "Superbrief descriptions." output)))))

;;; ---------------------------------------------------------------------------
;;; VERB REGISTRATION TESTS (vocabulary + syntax + dispatch)
;;; ---------------------------------------------------------------------------

(deftest verb-vocabulary-test
  (testing "verbose is registered in vocabulary as a verb"
    (is (= true (parser/wt? "verbose" :verb)))
    (is (= :verbose (parser/wt? "verbose" :verb true))))
  (testing "brief is registered in vocabulary as a verb"
    (is (= true (parser/wt? "brief" :verb)))
    (is (= :brief (parser/wt? "brief" :verb true))))
  (testing "superbrief is registered in vocabulary as a verb"
    (is (= true (parser/wt? "superbrief" :verb)))
    (is (= :super-brief (parser/wt? "superbrief" :verb true))))
  (testing "version is registered in vocabulary as a verb"
    (is (= true (parser/wt? "version" :verb)))
    (is (= :version (parser/wt? "version" :verb true)))))

(deftest verb-syntax-test
  (testing "verbose has a 0-argument syntax entry"
    (let [syntaxes (get verb-defs/*verb-syntaxes* :verbose)]
      (is (= 1 (count syntaxes)))
      (is (= 0 (:num-objects (first syntaxes))))
      (is (= :verbose (:action (first syntaxes))))))
  (testing "brief has a 0-argument syntax entry"
    (let [syntaxes (get verb-defs/*verb-syntaxes* :brief)]
      (is (= 1 (count syntaxes)))
      (is (= :brief (:action (first syntaxes)))))))

(deftest verb-parsing-test
  (testing "parsing 'verbose' sets prsa to :verbose"
    (let [gs (make-test-state)
          [_ result] (with-captured-output (parse-test-input gs "verbose"))]
      (is (nil? (get-in result [:parser :error])))
      (is (= :verbose (get-in result [:parser :prsa])))))
  (testing "parsing 'brief' sets prsa to :brief"
    (let [gs (make-test-state)
          [_ result] (with-captured-output (parse-test-input gs "brief"))]
      (is (nil? (get-in result [:parser :error])))
      (is (= :brief (get-in result [:parser :prsa]))))))

(deftest verb-dispatch-test
  (testing "perform dispatches :verbose to v-verbose"
    (let [gs (-> (make-test-state)
                 (assoc-in [:parser :prsa] :verbose))
          [output result] (with-captured-output (verb-defs/perform gs))]
      (is (= "Maximum verbosity." output))
      (is (= true (:verbose result)))))
  (testing "perform dispatches :brief to v-brief"
    (let [gs (-> (make-test-state)
                 (assoc-in [:parser :prsa] :brief))
          [output result] (with-captured-output (verb-defs/perform gs))]
      (is (= "Brief descriptions." output)))))

;;; ---------------------------------------------------------------------------
;;; INVENTORY VERB TESTS
;;; ---------------------------------------------------------------------------

(deftest v-inventory-empty-test
  (testing "v-inventory with empty inventory shows 'You are empty-handed.'"
    (let [gs (make-test-state)
          [output _] (with-captured-output (verbs/v-inventory gs))]
      (is (= "You are empty-handed." output)))))

(deftest v-inventory-with-items-test
  (testing "v-inventory with items shows 'You are carrying:' and lists items"
    (let [gs (-> (make-test-state)
                 ;; Add a lamp to the player's inventory
                 (gs/add-object {:id :brass-lantern
                                 :in :adventurer
                                 :desc "brass lantern"})
                 (gs/add-object {:id :leaflet
                                 :in :adventurer
                                 :desc "leaflet"}))
          [output _] (with-captured-output (verbs/v-inventory gs))]
      (is (clojure.string/includes? output "You are carrying:"))
      (is (clojure.string/includes? output "brass lantern"))
      (is (clojure.string/includes? output "leaflet")))))

(deftest inventory-vocabulary-test
  (testing "inventory is registered in vocabulary as a verb"
    (is (= true (parser/wt? "inventory" :verb)))
    (is (= :inventory (parser/wt? "inventory" :verb true))))
  (testing "i is a synonym for inventory"
    (is (= true (parser/wt? "i" :verb)))
    (is (= :inventory (parser/wt? "i" :verb true)))))

(deftest inventory-parsing-test
  (testing "parsing 'inventory' sets prsa to :inventory"
    (let [gs (make-test-state)
          [_ result] (with-captured-output (parse-test-input gs "inventory"))]
      (is (nil? (get-in result [:parser :error])))
      (is (= :inventory (get-in result [:parser :prsa])))))
  (testing "parsing 'i' sets prsa to :inventory"
    (let [gs (make-test-state)
          [_ result] (with-captured-output (parse-test-input gs "i"))]
      (is (nil? (get-in result [:parser :error])))
      (is (= :inventory (get-in result [:parser :prsa]))))))
