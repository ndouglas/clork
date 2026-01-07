(ns clork.verbs-test
  "Verb handler tests for Clork."
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clork.verbs-meta :as verbs-meta]
            [clork.verbs-health :as verbs-health]
            [clork.verbs-inventory :as verbs-inv]
            [clork.verbs-containers :as verbs-containers]
            [clork.verbs-movement :as verbs-movement]
            [clork.verbs-look :as verbs-look]
            [clork.verbs-put :as verbs-put]
            [clork.verb-defs :as verb-defs]
            [clork.parser :as parser]
            [clork.game-state :as gs]
            [clork.objects :as objects]
            [clork.rooms :as rooms]
            [clork.utils :as utils]
            [clork.utils-test :refer [with-captured-output make-test-state parse-test-input]]))

;;; ---------------------------------------------------------------------------
;;; VERB HANDLER TESTS
;;; ---------------------------------------------------------------------------

(deftest v-verbose-test
  (testing "v-verbose sets verbose to true and super-brief to false"
    (let [[output state] (with-captured-output (verbs-meta/v-verbose (gs/initial-game-state)))]
      (is (= true (:verbose state)))
      (is (= false (:super-brief state)))
      (is (= "Maximum verbosity." output)))))

(deftest v-brief-test
  (testing "v-brief sets verbose and super-brief to false"
    (let [[output state] (with-captured-output (verbs-meta/v-brief (gs/initial-game-state)))]
      (is (= false (:verbose state)))
      (is (= false (:super-brief state)))
      (is (= "Brief descriptions." output)))))

(deftest v-super-brief-test
  (testing "v-super-brief sets super-brief to true"
    (let [[output state] (with-captured-output (verbs-meta/v-super-brief (gs/initial-game-state)))]
      (is (= true (:super-brief state)))
      (is (= "Superbrief descriptions." output)))))

(deftest v-wait-test
  (testing "v-wait prints 'Time passes...' message"
    (let [gs (gs/initial-game-state)
          [output state] (with-captured-output (verbs-meta/v-wait gs))]
      (is (.contains output "Time passes..."))))

  (testing "v-wait sets clock-wait flag"
    (let [gs (gs/initial-game-state)
          [_ state] (with-captured-output (verbs-meta/v-wait gs))]
      (is (= true (:clock-wait state)))))

  (testing "v-wait increments moves counter"
    (let [gs (assoc (gs/initial-game-state) :moves 10)
          [_ state] (with-captured-output (verbs-meta/v-wait gs))]
      ;; Waits 1 turn by default, so moves should increase by 1
      (is (= 11 (:moves state)))))

  (testing "v-wait with custom turn count"
    (let [gs (assoc (gs/initial-game-state) :moves 5)
          [_ state] (with-captured-output (verbs-meta/v-wait gs 5))]
      ;; Waits 5 turns, so moves should increase by 5
      (is (= 10 (:moves state))))))

(deftest wait-verb-vocabulary-test
  (testing "wait is registered in vocabulary as a verb"
    (is (= true (parser/wt? "wait" :verb)))
    (is (= :wait (parser/wt? "wait" :verb true))))
  (testing "z is registered as synonym for wait"
    (is (= true (parser/wt? "z" :verb)))
    (is (= :wait (parser/wt? "z" :verb true)))))

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
          [output _] (with-captured-output (verbs-meta/v-inventory gs))]
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
          [output _] (with-captured-output (verbs-meta/v-inventory gs))]
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
          [output _] (with-captured-output (verbs-health/v-diagnose gs))]
      (is (clojure.string/includes? output "You are in perfect health.")))))

(deftest v-diagnose-light-wound-test
  (testing "v-diagnose with light wound reports it"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :adventurer :strength] -1))
          [output _] (with-captured-output (verbs-health/v-diagnose gs))]
      (is (clojure.string/includes? output "a light wound")))))

(deftest v-diagnose-serious-wound-test
  (testing "v-diagnose with serious wound reports it"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :adventurer :strength] -2))
          [output _] (with-captured-output (verbs-health/v-diagnose gs))]
      (is (clojure.string/includes? output "a serious wound")))))

(deftest v-diagnose-several-wounds-test
  (testing "v-diagnose with several wounds reports it"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :adventurer :strength] -3))
          [output _] (with-captured-output (verbs-health/v-diagnose gs))]
      (is (clojure.string/includes? output "several wounds")))))

(deftest v-diagnose-deaths-test
  (testing "v-diagnose reports death count when > 0"
    (let [gs (-> (make-test-state)
                 (assoc :deaths 1))
          [output _] (with-captured-output (verbs-health/v-diagnose gs))]
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
          [output _] (with-captured-output (verbs-health/v-score gs))]
      (is (clojure.string/includes? output "Your score is 0"))
      (is (clojure.string/includes? output "total of 350 points"))
      (is (clojure.string/includes? output "0 moves"))
      (is (clojure.string/includes? output "Beginner")))))

(deftest v-score-with-points-test
  (testing "v-score shows score and appropriate rank"
    (let [gs (-> (make-test-state)
                 (assoc :score 150)
                 (assoc :moves 42))
          [output _] (with-captured-output (verbs-health/v-score gs))]
      (is (clojure.string/includes? output "Your score is 150"))
      (is (clojure.string/includes? output "42 moves"))
      (is (clojure.string/includes? output "Junior Adventurer")))))

(deftest v-score-ranks-test
  (testing "v-score shows correct ranks at different score levels"
    ;; Beginner (0-25)
    (let [[output _] (with-captured-output (verbs-health/v-score (assoc (make-test-state) :score 10)))]
      (is (clojure.string/includes? output "Beginner")))
    ;; Amateur Adventurer (26-50)
    (let [[output _] (with-captured-output (verbs-health/v-score (assoc (make-test-state) :score 30)))]
      (is (clojure.string/includes? output "Amateur Adventurer")))
    ;; Novice Adventurer (51-100)
    (let [[output _] (with-captured-output (verbs-health/v-score (assoc (make-test-state) :score 75)))]
      (is (clojure.string/includes? output "Novice Adventurer")))
    ;; Junior Adventurer (101-200)
    (let [[output _] (with-captured-output (verbs-health/v-score (assoc (make-test-state) :score 150)))]
      (is (clojure.string/includes? output "Junior Adventurer")))
    ;; Adventurer (201-300)
    (let [[output _] (with-captured-output (verbs-health/v-score (assoc (make-test-state) :score 250)))]
      (is (clojure.string/includes? output "Adventurer")))
    ;; Master (301-330)
    (let [[output _] (with-captured-output (verbs-health/v-score (assoc (make-test-state) :score 320)))]
      (is (clojure.string/includes? output "Master")))
    ;; Wizard (331-349)
    (let [[output _] (with-captured-output (verbs-health/v-score (assoc (make-test-state) :score 340)))]
      (is (clojure.string/includes? output "Wizard")))
    ;; Master Adventurer (350)
    (let [[output _] (with-captured-output (verbs-health/v-score (assoc (make-test-state) :score 350)))]
      (is (clojure.string/includes? output "Master Adventurer")))))

(deftest v-score-move-singular-test
  (testing "v-score uses singular 'move' for 1 move"
    (let [gs (-> (make-test-state)
                 (assoc :moves 1))
          [output _] (with-captured-output (verbs-health/v-score gs))]
      (is (clojure.string/includes? output "1 move.")))))

(deftest score-upd-test
  (testing "score-upd adds to both score and base-score"
    (let [gs (-> (make-test-state)
                 (verbs-health/score-upd 10))]
      (is (= 10 (:score gs)))
      (is (= 10 (:base-score gs)))))
  (testing "score-upd can subtract points"
    (let [gs (-> (make-test-state)
                 (assoc :score 50 :base-score 50)
                 (verbs-health/score-upd -10))]
      (is (= 40 (:score gs)))
      (is (= 40 (:base-score gs))))))

(deftest score-obj-test
  (testing "score-obj scores an object's value and sets it to 0"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :diamond :value 10})
                 (verbs-health/score-obj :diamond))]
      (is (= 10 (:score gs)))
      (is (= 0 (get-in gs [:objects :diamond :value])))))
  (testing "score-obj does nothing if object has no value"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :rock :value 0})
                 (verbs-health/score-obj :rock))]
      (is (= 0 (:score gs)))))
  (testing "score-obj only scores object once"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :diamond :value 10})
                 (verbs-health/score-obj :diamond)
                 (verbs-health/score-obj :diamond))]
      (is (= 10 (:score gs))))))

(deftest room-entry-scoring-test
  (testing "entering a room with :value awards points"
    ;; Create a minimal game state with a room that has :value
    ;; Note: rooms must have :lit flag or player will be eaten by grue when walking
    (let [test-room {:id :treasure-vault :desc "Vault" :value 50 :exits {} :flags #{:lit}}
          start-room {:id :hallway :desc "Hallway" :exits {:north :treasure-vault} :flags #{:lit}}
          gs (-> (make-test-state)
                 (gs/add-rooms [test-room start-room])
                 (assoc :here :hallway)
                 (assoc-in [:objects :adventurer :in] :hallway)
                 (assoc-in [:parser :prso] :north))
          [_ result] (with-captured-output (verbs-movement/v-walk gs))]
      (is (= 50 (:score result)) "Entering room with :value should award points")
      (is (= 0 (get-in result [:rooms :treasure-vault :value]))
          "Room :value should be set to 0 after scoring")))

  (testing "re-entering the same room doesn't award points again"
    ;; Create a room with value, enter twice
    (let [test-room {:id :treasure-vault :desc "Vault" :value 50 :exits {:south :hallway} :flags #{:lit}}
          start-room {:id :hallway :desc "Hallway" :exits {:north :treasure-vault} :flags #{:lit}}
          gs (-> (make-test-state)
                 (gs/add-rooms [test-room start-room])
                 (assoc :here :hallway)
                 (assoc-in [:objects :adventurer :in] :hallway)
                 (assoc-in [:parser :prso] :north))
          ;; First entry
          [_ result1] (with-captured-output (verbs-movement/v-walk gs))
          ;; Go back and enter again
          [_ result2] (with-captured-output
                        (verbs-movement/v-walk (-> result1
                                          (assoc :here :treasure-vault)
                                          (assoc-in [:parser :prso] :south))))
          [_ result3] (with-captured-output
                        (verbs-movement/v-walk (-> result2
                                          (assoc :here :hallway)
                                          (assoc-in [:parser :prso] :north))))]
      (is (= 50 (:score result1)) "First entry should award 50 points")
      (is (= 50 (:score result3)) "Re-entering should not award more points")))

  (testing "rooms without :value don't affect score"
    ;; Create rooms without :value to test that no points are awarded
    (let [start-room {:id :test-start :desc "Start" :exits {:north :test-target} :flags #{:lit}}
          target-room {:id :test-target :desc "Target" :exits {} :flags #{:lit}} ;; No :value
          gs (-> (make-test-state)
                 (gs/add-rooms [start-room target-room])
                 (assoc :here :test-start)
                 (assoc-in [:objects :adventurer :in] :test-start)
                 (assoc-in [:parser :prso] :north))
          [_ result] (with-captured-output (verbs-movement/v-walk gs))]
      (is (= 0 (:score result)) "Entering room without :value should not change score"))))

;;; ---------------------------------------------------------------------------
;;; V-TAKE SCORING TESTS
;;; ---------------------------------------------------------------------------
;;; ZIL: ITAKE calls SCORE-OBJ when taking objects (gverbs.zil line 1977)

(deftest v-take-scores-treasure-test
  (testing "taking a treasure with :value awards points"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :egg :desc "jewel-encrusted egg"
                                 :in :west-of-house
                                 :flags #{:take}
                                 :value 5})
                 (assoc :here :west-of-house)
                 (assoc-in [:parser :prso] [:egg]))
          [_ result] (with-captured-output (verbs-inv/v-take gs))]
      (is (= 5 (:score result)) "Taking treasure should award its :value in points")
      (is (= 0 (get-in result [:objects :egg :value]))
          "Object :value should be set to 0 after scoring")))

  (testing "taking a non-treasure doesn't award points"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :rock :desc "rock"
                                 :in :west-of-house
                                 :flags #{:take}})
                 (assoc :here :west-of-house)
                 (assoc-in [:parser :prso] [:rock]))
          [_ result] (with-captured-output (verbs-inv/v-take gs))]
      (is (= 0 (:score result)) "Taking non-treasure should not award points")))

  (testing "taking the same treasure twice only scores once"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :diamond :desc "diamond"
                                 :in :west-of-house
                                 :flags #{:take}
                                 :value 10})
                 (assoc :here :west-of-house)
                 (assoc-in [:parser :prso] [:diamond]))
          ;; Take it once
          [_ result1] (with-captured-output (verbs-inv/v-take gs))
          ;; Drop it and take again
          dropped (-> result1
                      (assoc-in [:objects :diamond :in] :west-of-house)
                      (assoc-in [:parser :prso] [:diamond]))
          [_ result2] (with-captured-output (verbs-inv/v-take dropped))]
      (is (= 10 (:score result1)) "First take should award points")
      (is (= 10 (:score result2)) "Second take should not award more points"))))

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
    (binding [verbs-health/*read-input-fn* (constantly "y")]
      (let [gs (make-test-state)
            [output result] (with-captured-output (verbs-health/v-quit gs))]
        (is (true? (:quit result)))
        ;; Should show score first
        (is (clojure.string/includes? output "Your score is"))))))

(deftest v-quit-confirmed-yes-test
  (testing "v-quit with 'yes' response sets :quit to true"
    (binding [verbs-health/*read-input-fn* (constantly "yes")]
      (let [gs (make-test-state)
            [_ result] (with-captured-output (verbs-health/v-quit gs))]
        (is (true? (:quit result)))))))

(deftest v-quit-confirmed-uppercase-test
  (testing "v-quit with 'Y' response (uppercase) sets :quit to true"
    (binding [verbs-health/*read-input-fn* (constantly "Y")]
      (let [gs (make-test-state)
            [_ result] (with-captured-output (verbs-health/v-quit gs))]
        (is (true? (:quit result)))))))

(deftest v-quit-declined-test
  (testing "v-quit with 'n' response does not set :quit"
    (binding [verbs-health/*read-input-fn* (constantly "n")]
      (let [gs (make-test-state)
            [output result] (with-captured-output (verbs-health/v-quit gs))]
        (is (nil? (:quit result)))
        (is (clojure.string/includes? output "Ok."))))))

(deftest v-quit-declined-empty-test
  (testing "v-quit with empty response does not set :quit"
    (binding [verbs-health/*read-input-fn* (constantly "")]
      (let [gs (make-test-state)
            [output result] (with-captured-output (verbs-health/v-quit gs))]
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

;;; ---------------------------------------------------------------------------
;;; LOOK / DESCRIBE-OBJECTS VERB TESTS
;;; ---------------------------------------------------------------------------

(deftest see-inside-test
  (testing "see-inside? returns true for open containers"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :box
                                 :desc "box"
                                 :flags #{:cont :open}}))]
      (is (true? (verbs-look/see-inside? gs :box)))))
  (testing "see-inside? returns true for transparent containers"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :glass-box
                                 :desc "glass box"
                                 :flags #{:cont :trans}}))]
      (is (true? (verbs-look/see-inside? gs :glass-box)))))
  (testing "see-inside? returns false for closed opaque containers"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :chest
                                 :desc "chest"
                                 :flags #{:cont}}))]
      (is (false? (verbs-look/see-inside? gs :chest)))))
  (testing "see-inside? returns false for invisible objects"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :invisible-box
                                 :desc "invisible box"
                                 :flags #{:cont :open :invisible}}))]
      (is (false? (verbs-look/see-inside? gs :invisible-box))))))

(deftest describe-objects-basic-test
  (testing "describe-objects shows objects in lit room"
    (let [gs (-> (make-test-state)
                 (gs/set-flag :rooms :west-of-house :lit)
                 (gs/add-object {:id :lamp
                                 :in :west-of-house
                                 :desc "brass lamp"}))
          [output _] (with-captured-output (verbs-look/describe-objects gs true))]
      (is (clojure.string/includes? output "brass lamp")))))

(deftest describe-objects-dark-test
  (testing "describe-objects in dark shows bat message"
    (let [gs (-> (make-test-state)
                 ;; Make the room dark by removing the :lit flag
                 (assoc-in [:rooms :west-of-house :flags] #{})
                 (gs/add-object {:id :lamp
                                 :in :west-of-house
                                 :desc "brass lamp"}))
          [output _] (with-captured-output (verbs-look/describe-objects gs true))]
      (is (clojure.string/includes? output "Only bats can see in the dark")))))

(deftest describe-objects-with-fdesc-test
  (testing "describe-objects uses fdesc for untouched objects"
    (let [gs (-> (make-test-state)
                 (gs/set-flag :rooms :west-of-house :lit)
                 (gs/add-object {:id :lamp
                                 :in :west-of-house
                                 :desc "brass lamp"
                                 :fdesc "A brass lamp sits on the ground."}))
          [output _] (with-captured-output (verbs-look/describe-objects gs true))]
      (is (clojure.string/includes? output "A brass lamp sits on the ground.")))))

(deftest describe-objects-touched-test
  (testing "describe-objects uses generic description for touched objects without ldesc"
    (let [gs (-> (make-test-state)
                 (gs/set-flag :rooms :west-of-house :lit)
                 (gs/add-object {:id :lamp
                                 :in :west-of-house
                                 :desc "brass lamp"
                                 :flags #{:touch}
                                 :fdesc "A brass lamp sits on the ground."}))
          [output _] (with-captured-output (verbs-look/describe-objects gs true))]
      (is (clojure.string/includes? output "There is a brass lamp here")))))

(deftest describe-objects-container-test
  (testing "describe-objects shows contents of open containers"
    (let [gs (-> (make-test-state)
                 (gs/set-flag :rooms :west-of-house :lit)
                 (gs/add-object {:id :box
                                 :in :west-of-house
                                 :desc "wooden box"
                                 :flags #{:cont :open :touch}})
                 (gs/add-object {:id :coin
                                 :in :box
                                 :desc "gold coin"}))
          [output _] (with-captured-output (verbs-look/describe-objects gs true))]
      (is (clojure.string/includes? output "gold coin"))
      (is (clojure.string/includes? output "wooden box contains")))))

(deftest describe-objects-invisible-test
  (testing "describe-objects skips invisible objects"
    (let [gs (-> (make-test-state)
                 (gs/set-flag :rooms :west-of-house :lit)
                 (gs/add-object {:id :hidden
                                 :in :west-of-house
                                 :desc "hidden thing"
                                 :flags #{:invisible}}))
          [output _] (with-captured-output (verbs-look/describe-objects gs true))]
      (is (not (clojure.string/includes? output "hidden thing"))))))

(deftest describe-object-lighting-test
  (testing "describe-object shows (providing light) for lit objects"
    (let [gs (-> (make-test-state)
                 (gs/set-flag :rooms :west-of-house :lit)
                 (gs/add-object {:id :lamp
                                 :in :west-of-house
                                 :desc "brass lamp"
                                 :flags #{:on :touch}}))
          [output _] (with-captured-output (verbs-look/describe-objects gs true))]
      (is (clojure.string/includes? output "(providing light)")))))

(deftest look-vocabulary-test
  (testing "look is registered in vocabulary as a verb"
    (is (= true (parser/wt? "look" :verb)))
    (is (= :look (parser/wt? "look" :verb true))))
  (testing "l is a synonym for look"
    (is (= true (parser/wt? "l" :verb)))
    (is (= :look (parser/wt? "l" :verb true)))))

(deftest v-look-ldesc-newline-test
  (testing "v-look adds newline after ldesc before objects"
    (let [gs (-> (make-test-state)
                 (assoc :here :test-room)
                 (gs/add-room {:id :test-room
                               :desc "Test Room"
                               :ldesc "This is a test room."
                               :flags #{:lit}})
                 (gs/add-object {:id :ball
                                 :in :test-room
                                 :desc "red ball"
                                 :flags #{:touch}}))
          [output _] (with-captured-output (verbs-look/v-look gs))]
      ;; Room description should end with newline before object list
      (is (clojure.string/includes? output "This is a test room.\n"))
      ;; Object should be listed separately
      (is (clojure.string/includes? output "There is a red ball here")))))

;;; ---------------------------------------------------------------------------
;;; OPEN VERB TESTS
;;; ---------------------------------------------------------------------------

(deftest v-open-container-empty-test
  (testing "v-open on empty container sets :open flag and shows 'Opened.'"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :chest
                                 :in :west-of-house
                                 :desc "wooden chest"
                                 :flags #{:cont}})
                 (assoc-in [:parser :prso] :chest))
          [output result] (with-captured-output (verbs-containers/v-open gs))]
      (is (= "Opened." output))
      (is (contains? (get-in result [:objects :chest :flags]) :open))
      (is (contains? (get-in result [:objects :chest :flags]) :touch)))))

(deftest v-open-container-with-items-test
  (testing "v-open on container with items reveals contents"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :chest
                                 :in :west-of-house
                                 :desc "wooden chest"
                                 :flags #{:cont}})
                 (gs/add-object {:id :gold-coin
                                 :in :chest
                                 :desc "gold coin"})
                 (assoc-in [:parser :prso] :chest))
          [output result] (with-captured-output (verbs-containers/v-open gs))]
      (is (clojure.string/includes? output "Opening the wooden chest reveals"))
      (is (clojure.string/includes? output "gold coin"))
      (is (contains? (get-in result [:objects :chest :flags]) :open)))))

(deftest v-open-door-test
  (testing "v-open on door sets :open flag and shows 'The [door] opens.'"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :front-door
                                 :in :west-of-house
                                 :desc "front door"
                                 :flags #{:door}})
                 (assoc-in [:parser :prso] :front-door))
          [output result] (with-captured-output (verbs-containers/v-open gs))]
      (is (= "The front door opens." output))
      (is (contains? (get-in result [:objects :front-door :flags]) :open)))))

(deftest v-open-already-open-container-test
  (testing "v-open on already-open container shows 'It is already open.'"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :chest
                                 :in :west-of-house
                                 :desc "wooden chest"
                                 :flags #{:cont :open}})
                 (assoc-in [:parser :prso] :chest))
          [output _] (with-captured-output (verbs-containers/v-open gs))]
      (is (= "It is already open." output)))))

(deftest v-open-already-open-door-test
  (testing "v-open on already-open door shows 'It is already open.'"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :front-door
                                 :in :west-of-house
                                 :desc "front door"
                                 :flags #{:door :open}})
                 (assoc-in [:parser :prso] :front-door))
          [output _] (with-captured-output (verbs-containers/v-open gs))]
      (is (= "It is already open." output)))))

(deftest v-open-not-openable-test
  (testing "v-open on non-openable object shows error message"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :rock
                                 :in :west-of-house
                                 :desc "rock"
                                 :flags #{}})
                 (assoc-in [:parser :prso] :rock))
          [output _] (with-captured-output (verbs-containers/v-open gs))]
      (is (= "You must tell me how to do that to a rock." output)))))

(deftest v-open-transparent-container-test
  (testing "v-open on transparent empty container shows 'Opened.'"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :glass-box
                                 :in :west-of-house
                                 :desc "glass box"
                                 :flags #{:cont :trans}})
                 (assoc-in [:parser :prso] :glass-box))
          [output result] (with-captured-output (verbs-containers/v-open gs))]
      (is (= "Opened." output))
      (is (contains? (get-in result [:objects :glass-box :flags]) :open)))))

(deftest open-vocabulary-test
  (testing "open is registered in vocabulary as a verb"
    (is (= true (parser/wt? "open" :verb)))
    (is (= :open (parser/wt? "open" :verb true)))))

(deftest ^:pending open-parsing-test
  ;; Pending: requires object vocabulary registration to be implemented
  ;; When object synonyms are added to the global vocabulary, this test should pass
  (testing "parsing 'open mailbox' sets prsa to :open and prso to :mailbox"
    (let [gs (make-test-state)
          [_ result] (with-captured-output (parse-test-input gs "open mailbox"))]
      (is (nil? (get-in result [:parser :error])))
      (is (= :open (get-in result [:parser :prsa])))
      (is (= :mailbox (get-in result [:parser :prso]))))))

;;; ---------------------------------------------------------------------------
;;; EXAMINE VERB TESTS
;;; ---------------------------------------------------------------------------

(deftest v-examine-with-text-test
  (testing "v-examine shows object's text property when present"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :leaflet
                                 :in :west-of-house
                                 :desc "leaflet"
                                 :text "WELCOME TO ZORK!"})
                 (assoc-in [:parser :prso] :leaflet))
          [output _] (with-captured-output (verbs-containers/v-examine gs))]
      (is (= "WELCOME TO ZORK!" output)))))

(deftest v-examine-container-open-with-contents-test
  (testing "v-examine on open container with contents shows contents"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :chest
                                 :in :west-of-house
                                 :desc "wooden chest"
                                 :flags #{:cont :open}})
                 (gs/add-object {:id :gold-coin
                                 :in :chest
                                 :desc "gold coin"})
                 (assoc-in [:parser :prso] :chest))
          [output _] (with-captured-output (verbs-containers/v-examine gs))]
      (is (clojure.string/includes? output "wooden chest contains"))
      (is (clojure.string/includes? output "gold coin")))))

(deftest v-examine-container-open-empty-test
  (testing "v-examine on open empty container shows 'empty'"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :chest
                                 :in :west-of-house
                                 :desc "wooden chest"
                                 :flags #{:cont :open}})
                 (assoc-in [:parser :prso] :chest))
          [output _] (with-captured-output (verbs-containers/v-examine gs))]
      (is (= "The wooden chest is empty." output)))))

(deftest v-examine-container-closed-test
  (testing "v-examine on closed container shows 'closed'"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :chest
                                 :in :west-of-house
                                 :desc "wooden chest"
                                 :flags #{:cont}})
                 (assoc-in [:parser :prso] :chest))
          [output _] (with-captured-output (verbs-containers/v-examine gs))]
      (is (= "The wooden chest is closed." output)))))

(deftest v-examine-container-transparent-test
  (testing "v-examine on transparent container shows contents"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :glass-box
                                 :in :west-of-house
                                 :desc "glass box"
                                 :flags #{:cont :trans}})
                 (gs/add-object {:id :diamond
                                 :in :glass-box
                                 :desc "diamond"})
                 (assoc-in [:parser :prso] :glass-box))
          [output _] (with-captured-output (verbs-containers/v-examine gs))]
      (is (clojure.string/includes? output "glass box contains"))
      (is (clojure.string/includes? output "diamond")))))

(deftest v-examine-door-open-test
  (testing "v-examine on open door shows appropriate message"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :front-door
                                 :in :west-of-house
                                 :desc "front door"
                                 :flags #{:door :open}})
                 (assoc-in [:parser :prso] :front-door))
          [output _] (with-captured-output (verbs-containers/v-examine gs))]
      (is (= "The front door is open, but I can't tell what's beyond it." output)))))

(deftest v-examine-door-closed-test
  (testing "v-examine on closed door shows 'closed'"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :front-door
                                 :in :west-of-house
                                 :desc "front door"
                                 :flags #{:door}})
                 (assoc-in [:parser :prso] :front-door))
          [output _] (with-captured-output (verbs-containers/v-examine gs))]
      (is (= "The front door is closed." output)))))

(deftest v-examine-nothing-special-test
  (testing "v-examine on plain object shows 'nothing special'"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :rock
                                 :in :west-of-house
                                 :desc "rock"})
                 (assoc-in [:parser :prso] :rock))
          [output _] (with-captured-output (verbs-containers/v-examine gs))]
      (is (= "There's nothing special about the rock." output)))))

(deftest examine-vocabulary-test
  (testing "examine is registered in vocabulary as a verb"
    (is (= true (parser/wt? "examine" :verb)))
    (is (= :examine (parser/wt? "examine" :verb true))))
  (testing "x is a synonym for examine"
    (is (= true (parser/wt? "x" :verb)))
    (is (= :examine (parser/wt? "x" :verb true))))
  (testing "describe is a synonym for examine"
    (is (= true (parser/wt? "describe" :verb)))
    (is (= :examine (parser/wt? "describe" :verb true)))))

;;; ---------------------------------------------------------------------------
;;; LOOK-INSIDE VERB TESTS
;;; ---------------------------------------------------------------------------

(deftest v-look-inside-container-with-contents-test
  (testing "v-look-inside on open container shows contents"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :chest
                                 :in :west-of-house
                                 :desc "wooden chest"
                                 :flags #{:cont :open}})
                 (gs/add-object {:id :gold-coin
                                 :in :chest
                                 :desc "gold coin"})
                 (assoc-in [:parser :prso] :chest))
          [output _] (with-captured-output (verbs-containers/v-look-inside gs))]
      (is (clojure.string/includes? output "wooden chest contains"))
      (is (clojure.string/includes? output "gold coin")))))

(deftest v-look-inside-not-container-test
  (testing "v-look-inside on non-container shows error"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :rock
                                 :in :west-of-house
                                 :desc "rock"})
                 (assoc-in [:parser :prso] :rock))
          [output _] (with-captured-output (verbs-containers/v-look-inside gs))]
      (is (= "You can't look inside a rock." output)))))

(deftest v-look-inside-actor-test
  (testing "v-look-inside on actor shows special message"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :troll
                                 :in :west-of-house
                                 :desc "troll"
                                 :flags #{:cont :actor}})
                 (assoc-in [:parser :prso] :troll))
          [output _] (with-captured-output (verbs-containers/v-look-inside gs))]
      (is (= "There is nothing special to be seen." output)))))

(deftest look-inside-vocabulary-test
  (testing "search is registered in vocabulary as a verb"
    (is (= true (parser/wt? "search" :verb)))
    (is (= :look-inside (parser/wt? "search" :verb true)))))

;;; ---------------------------------------------------------------------------
;;; VERIFY VERB TESTS
;;; ---------------------------------------------------------------------------

(deftest v-verify-test
  (testing "v-verify always reports success"
    (let [gs (make-test-state)
          [output _] (with-captured-output (verbs-health/v-verify gs))]
      (is (clojure.string/includes? output "Verifying disk..."))
      (is (clojure.string/includes? output "The disk is correct.")))))

(deftest verify-vocabulary-test
  (testing "verify is registered in vocabulary as a verb"
    (is (= true (parser/wt? "verify" :verb)))
    (is (= :verify (parser/wt? "verify" :verb true))))
  (testing "$verify is a synonym for verify"
    (is (= true (parser/wt? "$verify" :verb)))
    (is (= :verify (parser/wt? "$verify" :verb true)))))

(deftest verify-parsing-test
  (testing "parsing 'verify' sets prsa to :verify"
    (let [gs (make-test-state)
          [_ result] (with-captured-output (parse-test-input gs "verify"))]
      (is (nil? (get-in result [:parser :error])))
      (is (= :verify (get-in result [:parser :prsa]))))))

;;; ---------------------------------------------------------------------------
;;; RESTART VERB TESTS
;;; ---------------------------------------------------------------------------

(deftest v-restart-confirmed-test
  (testing "v-restart with 'y' response sets :restart to true"
    (binding [verbs-health/*read-input-fn* (constantly "y")]
      (let [gs (make-test-state)
            [output result] (with-captured-output (verbs-health/v-restart gs))]
        (is (true? (:restart result)))
        ;; Should show score first
        (is (clojure.string/includes? output "Your score is"))
        (is (clojure.string/includes? output "Restarting."))))))

(deftest v-restart-declined-test
  (testing "v-restart with 'n' response does not set :restart"
    (binding [verbs-health/*read-input-fn* (constantly "n")]
      (let [gs (make-test-state)
            [output result] (with-captured-output (verbs-health/v-restart gs))]
        (is (nil? (:restart result)))
        (is (clojure.string/includes? output "Ok."))))))

(deftest restart-vocabulary-test
  (testing "restart is registered in vocabulary as a verb"
    (is (= true (parser/wt? "restart" :verb)))
    (is (= :restart (parser/wt? "restart" :verb true)))))

(deftest restart-parsing-test
  (testing "parsing 'restart' sets prsa to :restart"
    (let [gs (make-test-state)
          [_ result] (with-captured-output (parse-test-input gs "restart"))]
      (is (nil? (get-in result [:parser :error])))
      (is (= :restart (get-in result [:parser :prsa]))))))

;;; ---------------------------------------------------------------------------
;;; SAVE/RESTORE VERB TESTS
;;; ---------------------------------------------------------------------------

(deftest v-save-test
  (testing "v-save saves game state to file"
    (binding [verbs-health/*read-input-fn* (constantly "test-save.sav")]
      (let [gs (-> (make-test-state)
                   (assoc :score 42)
                   (assoc :moves 10))
            [output _] (with-captured-output (verbs-health/v-save gs))]
        (is (= "Ok." output))
        ;; Clean up test file
        (try
          (io/delete-file "test-save.sav")
          (catch Exception _))))))

(deftest v-restore-test
  (testing "v-restore restores game state from file"
    ;; First save a game
    (binding [verbs-health/*read-input-fn* (constantly "test-restore.sav")]
      (let [gs (-> (make-test-state)
                   (assoc :score 100)
                   (assoc :moves 50))]
        (with-captured-output (verbs-health/v-save gs))))
    ;; Now restore it
    (binding [verbs-health/*read-input-fn* (constantly "test-restore.sav")]
      (let [gs (-> (make-test-state)
                   (assoc :score 0)
                   (assoc :moves 0))
            [output result] (with-captured-output (verbs-health/v-restore gs))]
        (is (= "Ok." output))
        (is (true? (:restored result)))
        (is (= 100 (:score result)))
        (is (= 50 (:moves result)))))
    ;; Clean up
    (try
      (io/delete-file "test-restore.sav")
      (catch Exception _))))

(deftest v-restore-missing-file-test
  (testing "v-restore with missing file shows 'Failed.'"
    (binding [verbs-health/*read-input-fn* (constantly "nonexistent-file.sav")]
      (let [gs (make-test-state)
            [output result] (with-captured-output (verbs-health/v-restore gs))]
        (is (= "Failed." output))
        (is (nil? (:restored result)))))))

(deftest save-vocabulary-test
  (testing "save is registered in vocabulary as a verb"
    (is (= true (parser/wt? "save" :verb)))
    (is (= :save (parser/wt? "save" :verb true)))))

(deftest restore-vocabulary-test
  (testing "restore is registered in vocabulary as a verb"
    (is (= true (parser/wt? "restore" :verb)))
    (is (= :restore (parser/wt? "restore" :verb true)))))

;;; ---------------------------------------------------------------------------
;;; SCRIPT/UNSCRIPT VERB TESTS
;;; ---------------------------------------------------------------------------

(deftest v-script-test
  (testing "v-script starts transcription"
    (binding [verbs-health/*read-input-fn* (constantly "test-script.txt")]
      (let [gs (make-test-state)
            [output _] (with-captured-output (verbs-health/v-script gs))]
        (is (clojure.string/includes? output "Here begins a transcript"))
        (is (clojure.string/includes? output "ZORK I"))
        ;; Stop script and clean up
        (utils/stop-script!)
        (try
          (io/delete-file "test-script.txt")
          (catch Exception _))))))

(deftest v-script-already-on-test
  (testing "v-script when already on shows error"
    (binding [verbs-health/*read-input-fn* (constantly "test-script2.txt")]
      ;; Start script first
      (let [gs (make-test-state)]
        (with-captured-output (verbs-health/v-script gs)))
      ;; Try to start again
      (let [gs (make-test-state)
            [output _] (with-captured-output (verbs-health/v-script gs))]
        (is (= "Transcription is already on." output)))
      ;; Clean up
      (utils/stop-script!)
      (try
        (io/delete-file "test-script2.txt")
        (catch Exception _)))))

(deftest v-unscript-test
  (testing "v-unscript stops transcription"
    (binding [verbs-health/*read-input-fn* (constantly "test-unscript.txt")]
      ;; Start script first
      (let [gs (make-test-state)]
        (with-captured-output (verbs-health/v-script gs)))
      ;; Now stop it
      (let [gs (make-test-state)
            [output _] (with-captured-output (verbs-health/v-unscript gs))]
        (is (clojure.string/includes? output "Here ends a transcript"))
        (is (clojure.string/includes? output "ZORK I")))
      ;; Clean up
      (try
        (io/delete-file "test-unscript.txt")
        (catch Exception _)))))

(deftest v-unscript-not-on-test
  (testing "v-unscript when not on shows error"
    (let [gs (make-test-state)
          [output _] (with-captured-output (verbs-health/v-unscript gs))]
      (is (= "Transcription is not on." output)))))

(deftest script-vocabulary-test
  (testing "script is registered in vocabulary as a verb"
    (is (= true (parser/wt? "script" :verb)))
    (is (= :script (parser/wt? "script" :verb true)))))

(deftest unscript-vocabulary-test
  (testing "unscript is registered in vocabulary as a verb"
    (is (= true (parser/wt? "unscript" :verb)))
    (is (= :unscript (parser/wt? "unscript" :verb true)))))

;;; ---------------------------------------------------------------------------
;;; CLOSE VERB TESTS
;;; ---------------------------------------------------------------------------
;;; ZIL: V-CLOSE in gverbs.zil (line 352)

(deftest v-close-container-test
  (testing "v-close on open container clears :open flag and shows 'Closed.'"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :chest
                                 :in :west-of-house
                                 :desc "wooden chest"
                                 :flags #{:cont :open}})
                 (assoc-in [:parser :prso] :chest))
          [output result] (with-captured-output (verbs-containers/v-close gs))]
      (is (= "Closed." output))
      (is (not (contains? (get-in result [:objects :chest :flags]) :open))))))

(deftest v-close-container-already-closed-test
  (testing "v-close on already-closed container shows 'It is already closed.'"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :chest
                                 :in :west-of-house
                                 :desc "wooden chest"
                                 :flags #{:cont}})
                 (assoc-in [:parser :prso] :chest))
          [output _] (with-captured-output (verbs-containers/v-close gs))]
      (is (= "It is already closed." output)))))

(deftest v-close-door-test
  (testing "v-close on open door clears :open flag and shows 'The [door] is now closed.'"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :front-door
                                 :in :west-of-house
                                 :desc "front door"
                                 :flags #{:door :open}})
                 (assoc-in [:parser :prso] :front-door))
          [output result] (with-captured-output (verbs-containers/v-close gs))]
      (is (= "The front door is now closed." output))
      (is (not (contains? (get-in result [:objects :front-door :flags]) :open))))))

(deftest v-close-door-already-closed-test
  (testing "v-close on already-closed door shows 'It is already closed.'"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :front-door
                                 :in :west-of-house
                                 :desc "front door"
                                 :flags #{:door}})
                 (assoc-in [:parser :prso] :front-door))
          [output _] (with-captured-output (verbs-containers/v-close gs))]
      (is (= "It is already closed." output)))))

(deftest v-close-not-closable-test
  (testing "v-close on non-closable object shows error message"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :rock
                                 :in :west-of-house
                                 :desc "rock"
                                 :flags #{}})
                 (assoc-in [:parser :prso] :rock))
          [output _] (with-captured-output (verbs-containers/v-close gs))]
      (is (= "You must tell me how to do that to a rock." output)))))

(deftest v-close-surface-test
  (testing "v-close on surface shows 'You cannot close that.'"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :table
                                 :in :west-of-house
                                 :desc "table"
                                 :flags #{:cont :surface}})
                 (assoc-in [:parser :prso] :table))
          [output _] (with-captured-output (verbs-containers/v-close gs))]
      (is (= "You cannot close that." output)))))

(deftest close-vocabulary-test
  (testing "close is registered in vocabulary as a verb"
    (is (= true (parser/wt? "close" :verb)))
    (is (= :close (parser/wt? "close" :verb true))))
  (testing "shut is a synonym for close"
    (is (= true (parser/wt? "shut" :verb)))
    (is (= :close (parser/wt? "shut" :verb true)))))

;;; ---------------------------------------------------------------------------
;;; BACK VERB TESTS
;;; ---------------------------------------------------------------------------
;;; ZIL: V-BACK in gverbs.zil (line 211)
;;; Note: Original Zork I doesn't track previous room - just shows error message

(deftest v-back-test
  (testing "v-back shows 'Sorry, my memory is poor' message"
    (let [gs (make-test-state)
          [output _] (with-captured-output (verbs-movement/v-back gs))]
      (is (= "Sorry, my memory is poor. Please give a direction." output)))))

(deftest back-vocabulary-test
  (testing "back is registered in vocabulary as a verb"
    (is (= true (parser/wt? "back" :verb)))
    (is (= :back (parser/wt? "back" :verb true))))
  (testing "return is a synonym for back"
    (is (= true (parser/wt? "return" :verb)))
    (is (= :back (parser/wt? "return" :verb true)))))

;;; ---------------------------------------------------------------------------
;;; LOOK UNDER VERB TESTS
;;; ---------------------------------------------------------------------------
;;; ZIL: V-LOOK-UNDER in gverbs.zil (line 915)

(deftest v-look-under-test
  (testing "v-look-under shows 'There is nothing but dust there.'"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :table
                                 :in :west-of-house
                                 :desc "table"})
                 (assoc-in [:parser :prso] :table))
          [output _] (with-captured-output (verbs-containers/v-look-under gs))]
      (is (= "There is nothing but dust there." output)))))

(deftest look-under-vocabulary-test
  (testing "look-under action is reachable via 'look under' syntax"
    ;; The verb 'look' with prep 'under' routes to :look-under
    (is (= true (parser/wt? "look" :verb)))
    (is (= :look (parser/wt? "look" :verb true)))))

;;; ---------------------------------------------------------------------------
;;; LOOK BEHIND VERB TESTS
;;; ---------------------------------------------------------------------------
;;; ZIL: V-LOOK-BEHIND in gverbs.zil (line 878)

(deftest v-look-behind-test
  (testing "v-look-behind shows 'There is nothing behind the <object>.'"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :couch
                                 :in :west-of-house
                                 :desc "couch"})
                 (assoc-in [:parser :prso] :couch))
          [output _] (with-captured-output (verbs-containers/v-look-behind gs))]
      (is (= "There is nothing behind the couch." output)))))

;;; ---------------------------------------------------------------------------
;;; LOOK ON VERB TESTS
;;; ---------------------------------------------------------------------------
;;; ZIL: V-LOOK-ON in gverbs.zil (line 908)

(deftest v-look-on-surface-test
  (testing "v-look-on on surface delegates to v-look-inside"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :table
                                 :in :west-of-house
                                 :desc "table"
                                 :flags #{:cont :surface :open}})
                 (gs/add-object {:id :vase
                                 :in :table
                                 :desc "vase"})
                 (assoc-in [:parser :prso] :table))
          [output _] (with-captured-output (verbs-containers/v-look-on gs))]
      ;; Should show contents like v-look-inside does
      (is (clojure.string/includes? output "table contains"))
      (is (clojure.string/includes? output "vase")))))

(deftest v-look-on-surface-empty-test
  (testing "v-look-on on empty surface shows 'empty'"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :table
                                 :in :west-of-house
                                 :desc "table"
                                 :flags #{:cont :surface :open}})
                 (assoc-in [:parser :prso] :table))
          [output _] (with-captured-output (verbs-containers/v-look-on gs))]
      (is (= "The table is empty." output)))))

(deftest v-look-on-non-surface-test
  (testing "v-look-on on non-surface shows 'Look on a <object>???'"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :rock
                                 :in :west-of-house
                                 :desc "rock"})
                 (assoc-in [:parser :prso] :rock))
          [output _] (with-captured-output (verbs-containers/v-look-on gs))]
      (is (= "Look on a rock???" output)))))

;;; ---------------------------------------------------------------------------
;;; TROPHY CASE TESTS
;;; ---------------------------------------------------------------------------
;;; ZIL: TROPHY-CASE-FCN in 1actions.zil (lines 455-458)

(defn- make-trophy-case-state
  "Create a game state with trophy case, living room, and a treasure."
  []
  (-> (gs/initial-game-state)
      (gs/add-rooms [rooms/living-room])
      (gs/add-objects [objects/adventurer objects/trophy-case objects/bag-of-coins])
      (assoc :here :living-room)))

(deftest trophy-case-take-test
  (testing "Taking trophy case shows 'securely fastened' message"
    (let [gs (-> (make-trophy-case-state)
                 (assoc-in [:parser :prsa] :take)
                 (assoc-in [:parser :prso] :trophy-case))
          action (get-in gs [:objects :trophy-case :action])
          [output _] (with-captured-output (action gs))]
      (is (= "The trophy case is securely fastened to the wall." output)))))

(deftest trophy-case-tvalue-scoring-test
  (testing "Putting treasure in trophy case scores both :value and :tvalue"
    ;; Bag of coins has VALUE 10 and TVALUE 5
    (let [gs (-> (make-trophy-case-state)
                 ;; Put bag in player's inventory
                 (assoc-in [:objects :bag-of-coins :in] :adventurer)
                 ;; Open the trophy case
                 (gs/set-thing-flag :trophy-case :open)
                 ;; Set up parser for "put bag in case"
                 (assoc-in [:parser :prso] :bag-of-coins)
                 (assoc-in [:parser :prsi] :trophy-case))
          [_ result] (with-captured-output (verbs-put/v-put gs))]
      ;; Score should be VALUE (10) + TVALUE (5) = 15
      (is (= 15 (:score result)))
      ;; Object should now be in trophy case
      (is (= :trophy-case (get-in result [:objects :bag-of-coins :in])))
      ;; Both value and tvalue should be zeroed out
      (is (= 0 (get-in result [:objects :bag-of-coins :value])))
      (is (= 0 (get-in result [:objects :bag-of-coins :tvalue]))))))

(deftest trophy-case-only-scores-tvalue-for-case-test
  (testing "Putting treasure in other containers only scores :value, not :tvalue"
    (let [gs (-> (make-trophy-case-state)
                 ;; Add a different container (sack)
                 (gs/add-object {:id :sack
                                 :in :living-room
                                 :desc "sack"
                                 :flags #{:cont :open}
                                 :capacity 100})
                 ;; Put bag in player's inventory
                 (assoc-in [:objects :bag-of-coins :in] :adventurer)
                 ;; Set up parser for "put bag in sack"
                 (assoc-in [:parser :prso] :bag-of-coins)
                 (assoc-in [:parser :prsi] :sack))
          [_ result] (with-captured-output (verbs-put/v-put gs))]
      ;; Score should only be VALUE (10), not TVALUE
      (is (= 10 (:score result)))
      ;; Object should be in sack
      (is (= :sack (get-in result [:objects :bag-of-coins :in])))
      ;; VALUE should be zeroed, but TVALUE should remain for later trophy case deposit
      (is (= 0 (get-in result [:objects :bag-of-coins :value])))
      (is (= 5 (get-in result [:objects :bag-of-coins :tvalue]))))))

(deftest score-tvalue-test
  (testing "score-tvalue adds tvalue to score and zeroes it out"
    (let [gs (-> (make-trophy-case-state)
                 (verbs-health/score-tvalue :bag-of-coins))]
      ;; Should have added TVALUE (5) to score
      (is (= 5 (:score gs)))
      ;; TVALUE should now be 0
      (is (= 0 (get-in gs [:objects :bag-of-coins :tvalue])))))

  (testing "score-tvalue with zero tvalue does nothing"
    (let [gs (-> (make-trophy-case-state)
                 (assoc-in [:objects :bag-of-coins :tvalue] 0)
                 (verbs-health/score-tvalue :bag-of-coins))]
      ;; Score should remain 0
      (is (= 0 (:score gs))))))

;;; ---------------------------------------------------------------------------
;;; EAT/DRINK VERB TESTS
;;; ---------------------------------------------------------------------------
;;; ZIL: V-EAT in gverbs.zil (lines 499-532)
;;; ZIL: V-DRINK in gverbs.zil (lines 484-485)

(deftest eat-vocabulary-test
  (testing "eat is registered in vocabulary as a verb"
    (is (= true (parser/wt? "eat" :verb)))
    (is (= :eat (parser/wt? "eat" :verb true))))
  (testing "consume is a synonym for eat"
    (is (= true (parser/wt? "consume" :verb)))
    (is (= :eat (parser/wt? "consume" :verb true)))))

(deftest drink-vocabulary-test
  (testing "drink is registered in vocabulary as a verb"
    (is (= true (parser/wt? "drink" :verb)))
    (is (= :drink (parser/wt? "drink" :verb true))))
  (testing "imbibe is a synonym for drink"
    (is (= true (parser/wt? "imbibe" :verb)))
    (is (= :drink (parser/wt? "imbibe" :verb true)))))

(deftest v-eat-food-test
  (testing "eating food removes it from game"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :lunch
                                 :in :adventurer
                                 :desc "lunch"
                                 :flags #{:take :food}})
                 (assoc-in [:parser :prsa] :eat)
                 (assoc-in [:parser :prso] [:lunch]))
          v-eat (requiring-resolve 'clork.verbs-food/v-eat)
          [output result] (with-captured-output (v-eat gs))]
      (is (clojure.string/includes? output "hit the spot"))
      (is (nil? (get-in result [:objects :lunch :in]))))))

(deftest v-eat-not-holding-test
  (testing "eating food not held shows 'not holding' message"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :lunch
                                 :in :west-of-house
                                 :desc "lunch"
                                 :flags #{:take :food}})
                 (assoc-in [:parser :prsa] :eat)
                 (assoc-in [:parser :prso] [:lunch]))
          v-eat (requiring-resolve 'clork.verbs-food/v-eat)
          [output _] (with-captured-output (v-eat gs))]
      (is (clojure.string/includes? output "not holding")))))

(deftest v-drink-not-food-test
  (testing "trying to drink food shows 'How can you drink that?'"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :lunch
                                 :in :adventurer
                                 :desc "lunch"
                                 :flags #{:take :food}})
                 (assoc-in [:parser :prsa] :drink)
                 (assoc-in [:parser :prso] [:lunch]))
          v-eat (requiring-resolve 'clork.verbs-food/v-eat)
          [output _] (with-captured-output (v-eat gs))]
      (is (clojure.string/includes? output "How can you drink")))))

(deftest v-eat-non-food-test
  (testing "eating non-food shows 'would not agree' message"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :rock
                                 :in :adventurer
                                 :desc "rock"
                                 :flags #{:take}})
                 (assoc-in [:parser :prsa] :eat)
                 (assoc-in [:parser :prso] [:rock]))
          v-eat (requiring-resolve 'clork.verbs-food/v-eat)
          [output _] (with-captured-output (v-eat gs))]
      (is (clojure.string/includes? output "would agree with you")))))

;;; ---------------------------------------------------------------------------
;;; GIVE VERB TESTS
;;; ---------------------------------------------------------------------------
;;; ZIL: V-GIVE in gverbs.zil (lines 729-733)

(deftest give-vocabulary-test
  (testing "give is registered in vocabulary as a verb"
    (is (= true (parser/wt? "give" :verb)))
    (is (= :give (parser/wt? "give" :verb true))))
  (testing "hand is a synonym for give"
    (is (= true (parser/wt? "hand" :verb)))
    (is (= :give (parser/wt? "hand" :verb true)))))

(deftest v-give-to-actor-test
  (testing "giving to actor shows 'refuses politely' message"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :sword
                                 :in :adventurer
                                 :desc "sword"
                                 :flags #{:take}})
                 (gs/add-object {:id :troll
                                 :in :west-of-house
                                 :desc "troll"
                                 :flags #{:actor}})
                 (assoc-in [:parser :prsa] :give)
                 (assoc-in [:parser :prso] [:sword])
                 (assoc-in [:parser :prsi] [:troll]))
          v-give (requiring-resolve 'clork.verbs-food/v-give)
          [output _] (with-captured-output (v-give gs))]
      (is (clojure.string/includes? output "refuses it politely")))))

(deftest v-give-to-non-actor-test
  (testing "giving to non-actor shows 'can't give' message"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :sword
                                 :in :adventurer
                                 :desc "sword"
                                 :flags #{:take}})
                 (gs/add-object {:id :rock
                                 :in :west-of-house
                                 :desc "rock"
                                 :flags #{:take}})
                 (assoc-in [:parser :prsa] :give)
                 (assoc-in [:parser :prso] [:sword])
                 (assoc-in [:parser :prsi] [:rock]))
          v-give (requiring-resolve 'clork.verbs-food/v-give)
          [output _] (with-captured-output (v-give gs))]
      (is (clojure.string/includes? output "can't give")))))

;;; ---------------------------------------------------------------------------
;;; LOCK/UNLOCK VERB TESTS
;;; ---------------------------------------------------------------------------
;;; ZIL: V-LOCK in gverbs.zil (lines 871-872)
;;; ZIL: V-UNLOCK in gverbs.zil (lines 1524-1525)

(deftest lock-vocabulary-test
  (testing "lock is registered in vocabulary as a verb"
    (is (= true (parser/wt? "lock" :verb)))
    (is (= :lock (parser/wt? "lock" :verb true)))))

(deftest unlock-vocabulary-test
  (testing "unlock is registered in vocabulary as a verb"
    (is (= true (parser/wt? "unlock" :verb)))
    (is (= :unlock (parser/wt? "unlock" :verb true)))))

(deftest v-lock-default-test
  (testing "lock on non-lockable shows 'doesn't seem to work' message"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :door
                                 :in :west-of-house
                                 :desc "door"
                                 :flags #{:door}})
                 (gs/add-object {:id :key
                                 :in :adventurer
                                 :desc "key"
                                 :flags #{:tool :take}})
                 (assoc-in [:parser :prsa] :lock)
                 (assoc-in [:parser :prso] [:door])
                 (assoc-in [:parser :prsi] [:key]))
          v-lock (requiring-resolve 'clork.verbs-food/v-lock)
          [output _] (with-captured-output (v-lock gs))]
      (is (clojure.string/includes? output "doesn't seem to work")))))

(deftest v-unlock-default-test
  (testing "unlock on non-lockable shows 'doesn't seem to work' message"
    (let [gs (-> (make-test-state)
                 (gs/add-object {:id :door
                                 :in :west-of-house
                                 :desc "door"
                                 :flags #{:door}})
                 (gs/add-object {:id :key
                                 :in :adventurer
                                 :desc "key"
                                 :flags #{:tool :take}})
                 (assoc-in [:parser :prsa] :unlock)
                 (assoc-in [:parser :prso] [:door])
                 (assoc-in [:parser :prsi] [:key]))
          v-unlock (requiring-resolve 'clork.verbs-food/v-unlock)
          [output _] (with-captured-output (v-unlock gs))]
      (is (clojure.string/includes? output "doesn't seem to work")))))
