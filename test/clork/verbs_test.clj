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

;;; ---------------------------------------------------------------------------
;;; DIAGNOSE VERB TESTS
;;; ---------------------------------------------------------------------------

(deftest v-diagnose-perfect-health-test
  (testing "v-diagnose with no wounds shows 'You are in perfect health.'"
    (let [gs (make-test-state)
          [output _] (with-captured-output (verbs/v-diagnose gs))]
      (is (clojure.string/includes? output "You are in perfect health.")))))

(deftest v-diagnose-light-wound-test
  (testing "v-diagnose with light wound reports it"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :adventurer :strength] -1))
          [output _] (with-captured-output (verbs/v-diagnose gs))]
      (is (clojure.string/includes? output "a light wound")))))

(deftest v-diagnose-serious-wound-test
  (testing "v-diagnose with serious wound reports it"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :adventurer :strength] -2))
          [output _] (with-captured-output (verbs/v-diagnose gs))]
      (is (clojure.string/includes? output "a serious wound")))))

(deftest v-diagnose-several-wounds-test
  (testing "v-diagnose with several wounds reports it"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :adventurer :strength] -3))
          [output _] (with-captured-output (verbs/v-diagnose gs))]
      (is (clojure.string/includes? output "several wounds")))))

(deftest v-diagnose-deaths-test
  (testing "v-diagnose reports death count when > 0"
    (let [gs (-> (make-test-state)
                 (assoc :deaths 1))
          [output _] (with-captured-output (verbs/v-diagnose gs))]
      (is (clojure.string/includes? output "You have been killed once.")))))

(deftest diagnose-vocabulary-test
  (testing "diagnose is registered in vocabulary as a verb"
    (is (= true (parser/wt? "diagnose" :verb)))
    (is (= :diagnose (parser/wt? "diagnose" :verb true)))))

(deftest diagnose-parsing-test
  (testing "parsing 'diagnose' sets prsa to :diagnose"
    (let [gs (make-test-state)
          [_ result] (with-captured-output (parse-test-input gs "diagnose"))]
      (is (nil? (get-in result [:parser :error])))
      (is (= :diagnose (get-in result [:parser :prsa]))))))

;;; ---------------------------------------------------------------------------
;;; SCORE VERB TESTS
;;; ---------------------------------------------------------------------------

(deftest v-score-initial-test
  (testing "v-score shows initial score of 0 and rank of Beginner"
    (let [gs (make-test-state)
          [output _] (with-captured-output (verbs/v-score gs))]
      (is (clojure.string/includes? output "Your score is 0"))
      (is (clojure.string/includes? output "total of 350 points"))
      (is (clojure.string/includes? output "0 moves"))
      (is (clojure.string/includes? output "Beginner")))))

(deftest v-score-with-points-test
  (testing "v-score shows score and appropriate rank"
    (let [gs (-> (make-test-state)
                 (assoc :score 150)
                 (assoc :moves 42))
          [output _] (with-captured-output (verbs/v-score gs))]
      (is (clojure.string/includes? output "Your score is 150"))
      (is (clojure.string/includes? output "42 moves"))
      (is (clojure.string/includes? output "Junior Adventurer")))))

(deftest v-score-ranks-test
  (testing "v-score shows correct ranks at different score levels"
    ;; Beginner (0-25)
    (let [[output _] (with-captured-output (verbs/v-score (assoc (make-test-state) :score 10)))]
      (is (clojure.string/includes? output "Beginner")))
    ;; Amateur Adventurer (26-50)
    (let [[output _] (with-captured-output (verbs/v-score (assoc (make-test-state) :score 30)))]
      (is (clojure.string/includes? output "Amateur Adventurer")))
    ;; Novice Adventurer (51-100)
    (let [[output _] (with-captured-output (verbs/v-score (assoc (make-test-state) :score 75)))]
      (is (clojure.string/includes? output "Novice Adventurer")))
    ;; Junior Adventurer (101-200)
    (let [[output _] (with-captured-output (verbs/v-score (assoc (make-test-state) :score 150)))]
      (is (clojure.string/includes? output "Junior Adventurer")))
    ;; Adventurer (201-300)
    (let [[output _] (with-captured-output (verbs/v-score (assoc (make-test-state) :score 250)))]
      (is (clojure.string/includes? output "Adventurer")))
    ;; Master (301-330)
    (let [[output _] (with-captured-output (verbs/v-score (assoc (make-test-state) :score 320)))]
      (is (clojure.string/includes? output "Master")))
    ;; Wizard (331-349)
    (let [[output _] (with-captured-output (verbs/v-score (assoc (make-test-state) :score 340)))]
      (is (clojure.string/includes? output "Wizard")))
    ;; Master Adventurer (350)
    (let [[output _] (with-captured-output (verbs/v-score (assoc (make-test-state) :score 350)))]
      (is (clojure.string/includes? output "Master Adventurer")))))

(deftest v-score-move-singular-test
  (testing "v-score uses singular 'move' for 1 move"
    (let [gs (-> (make-test-state)
                 (assoc :moves 1))
          [output _] (with-captured-output (verbs/v-score gs))]
      (is (clojure.string/includes? output "1 move.")))))

(deftest score-upd-test
  (testing "score-upd adds to both score and base-score"
    (let [gs (-> (make-test-state)
                 (verbs/score-upd 10))]
      (is (= 10 (:score gs)))
      (is (= 10 (:base-score gs)))))
  (testing "score-upd can subtract points"
    (let [gs (-> (make-test-state)
                 (assoc :score 50 :base-score 50)
                 (verbs/score-upd -10))]
      (is (= 40 (:score gs)))
      (is (= 40 (:base-score gs))))))

(deftest score-obj-test
  (testing "score-obj scores an object's value and sets it to 0"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :diamond :value 10})
                 (verbs/score-obj :diamond))]
      (is (= 10 (:score gs)))
      (is (= 0 (get-in gs [:objects :diamond :value])))))
  (testing "score-obj does nothing if object has no value"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :rock :value 0})
                 (verbs/score-obj :rock))]
      (is (= 0 (:score gs)))))
  (testing "score-obj only scores object once"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :diamond :value 10})
                 (verbs/score-obj :diamond)
                 (verbs/score-obj :diamond))]
      (is (= 10 (:score gs))))))

(deftest score-vocabulary-test
  (testing "score is registered in vocabulary as a verb"
    (is (= true (parser/wt? "score" :verb)))
    (is (= :score (parser/wt? "score" :verb true)))))

(deftest score-parsing-test
  (testing "parsing 'score' sets prsa to :score"
    (let [gs (make-test-state)
          [_ result] (with-captured-output (parse-test-input gs "score"))]
      (is (nil? (get-in result [:parser :error])))
      (is (= :score (get-in result [:parser :prsa]))))))

;;; ---------------------------------------------------------------------------
;;; QUIT VERB TESTS
;;; ---------------------------------------------------------------------------

(deftest v-quit-confirmed-test
  (testing "v-quit with 'y' response sets :quit to true"
    (binding [verbs/*read-input-fn* (constantly "y")]
      (let [gs (make-test-state)
            [output result] (with-captured-output (verbs/v-quit gs))]
        (is (true? (:quit result)))
        ;; Should show score first
        (is (clojure.string/includes? output "Your score is"))))))

(deftest v-quit-confirmed-yes-test
  (testing "v-quit with 'yes' response sets :quit to true"
    (binding [verbs/*read-input-fn* (constantly "yes")]
      (let [gs (make-test-state)
            [_ result] (with-captured-output (verbs/v-quit gs))]
        (is (true? (:quit result)))))))

(deftest v-quit-confirmed-uppercase-test
  (testing "v-quit with 'Y' response (uppercase) sets :quit to true"
    (binding [verbs/*read-input-fn* (constantly "Y")]
      (let [gs (make-test-state)
            [_ result] (with-captured-output (verbs/v-quit gs))]
        (is (true? (:quit result)))))))

(deftest v-quit-declined-test
  (testing "v-quit with 'n' response does not set :quit"
    (binding [verbs/*read-input-fn* (constantly "n")]
      (let [gs (make-test-state)
            [output result] (with-captured-output (verbs/v-quit gs))]
        (is (nil? (:quit result)))
        (is (clojure.string/includes? output "Ok."))))))

(deftest v-quit-declined-empty-test
  (testing "v-quit with empty response does not set :quit"
    (binding [verbs/*read-input-fn* (constantly "")]
      (let [gs (make-test-state)
            [output result] (with-captured-output (verbs/v-quit gs))]
        (is (nil? (:quit result)))
        (is (clojure.string/includes? output "Ok."))))))

(deftest quit-vocabulary-test
  (testing "quit is registered in vocabulary as a verb"
    (is (= true (parser/wt? "quit" :verb)))
    (is (= :quit (parser/wt? "quit" :verb true))))
  (testing "q is a synonym for quit"
    (is (= true (parser/wt? "q" :verb)))
    (is (= :quit (parser/wt? "q" :verb true)))))

(deftest quit-parsing-test
  (testing "parsing 'quit' sets prsa to :quit"
    (let [gs (make-test-state)
          [_ result] (with-captured-output (parse-test-input gs "quit"))]
      (is (nil? (get-in result [:parser :error])))
      (is (= :quit (get-in result [:parser :prsa])))))
  (testing "parsing 'q' sets prsa to :quit"
    (let [gs (make-test-state)
          [_ result] (with-captured-output (parse-test-input gs "q"))]
      (is (nil? (get-in result [:parser :error])))
      (is (= :quit (get-in result [:parser :prsa]))))))
