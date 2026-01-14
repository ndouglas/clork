(ns clork.game-state-test
  "Game state tests for Clork."
  (:require [clojure.test :refer :all]
            [clork.game-state :as gs]))

;; Generic flag functions

(deftest set-flag-test
  (testing "set-flag sets a flag on an object"
    (let [game-state {:objects {:obj1 {}}}
          result (gs/set-flag game-state :objects :obj1 :takeable)]
      (is (= {:obj1 {:takeable true}} (:objects result)))))
  (testing "set-flag sets a flag on a room"
    (let [game-state {:rooms {:room1 {}}}
          result (gs/set-flag game-state :rooms :room1 :lit)]
      (is (= {:room1 {:lit true}} (:rooms result))))))

(deftest unset-flag-test
  (testing "unset-flag unsets a flag on an object"
    (let [game-state {:objects {:obj1 {:takeable true}}}
          result (gs/unset-flag game-state :objects :obj1 :takeable)]
      (is (= {:obj1 {:takeable false}} (:objects result)))))
  (testing "unset-flag unsets a flag on a room"
    (let [game-state {:rooms {:room1 {:lit true}}}
          result (gs/unset-flag game-state :rooms :room1 :lit)]
      (is (= {:room1 {:lit false}} (:rooms result))))))

(deftest flag?-test
  (testing "flag? returns true when flag is set as direct key"
    (is (gs/flag? {:objects {:obj1 {:takeable true}}} :objects :obj1 :takeable))
    (is (gs/flag? {:rooms {:room1 {:lit true}}} :rooms :room1 :lit)))
  (testing "flag? returns true when flag is in :flags set"
    (is (gs/flag? {:objects {:obj1 {:flags #{:takeable :cont}}}} :objects :obj1 :takeable))
    (is (gs/flag? {:objects {:obj1 {:flags #{:takeable :cont}}}} :objects :obj1 :cont))
    (is (gs/flag? {:rooms {:room1 {:flags #{:lit :on}}}} :rooms :room1 :lit)))
  (testing "flag? returns true when flag is set both ways (direct key takes precedence)"
    (is (gs/flag? {:objects {:obj1 {:takeable true :flags #{:cont}}}} :objects :obj1 :takeable))
    (is (gs/flag? {:objects {:obj1 {:takeable true :flags #{:cont}}}} :objects :obj1 :cont)))
  (testing "flag? returns false when flag is not set"
    (is (not (gs/flag? {:objects {:obj1 {:takeable false}}} :objects :obj1 :takeable)))
    (is (not (gs/flag? {:objects {:obj1 {}}} :objects :obj1 :takeable)))
    (is (not (gs/flag? {:objects {:obj1 {:flags #{:cont}}}} :objects :obj1 :takeable)))))

;; Polymorphic thing-flag functions

(deftest set-thing-flag-test
  (testing "set-thing-flag sets a flag on an object"
    (let [game-state {:objects {:lamp {}}}
          result (gs/set-thing-flag game-state :lamp :on)]
      (is (= {:lamp {:on true}} (:objects result)))))
  (testing "set-thing-flag sets a flag on a room"
    (let [game-state {:rooms {:kitchen {}}}
          result (gs/set-thing-flag game-state :kitchen :visited)]
      (is (= {:kitchen {:visited true}} (:rooms result)))))
  (testing "set-thing-flag throws for unknown thing"
    (let [game-state {:objects {} :rooms {}}]
      (is (thrown? Exception (gs/set-thing-flag game-state :unknown :flag))))))

(deftest unset-thing-flag-test
  (testing "unset-thing-flag unsets a flag on an object"
    (let [game-state {:objects {:lamp {:on true}}}
          result (gs/unset-thing-flag game-state :lamp :on)]
      (is (= {:lamp {:on false}} (:objects result)))))
  (testing "unset-thing-flag unsets a flag on a room"
    (let [game-state {:rooms {:kitchen {:visited true}}}
          result (gs/unset-thing-flag game-state :kitchen :visited)]
      (is (= {:kitchen {:visited false}} (:rooms result))))))

(deftest set-thing-flag?-test
  (testing "set-thing-flag? returns true when flag is set"
    (is (gs/set-thing-flag? {:objects {:lamp {:on true}}} :lamp :on))
    (is (gs/set-thing-flag? {:rooms {:kitchen {:lit true}}} :kitchen :lit)))
  (testing "set-thing-flag? returns false when flag is not set"
    (is (not (gs/set-thing-flag? {:objects {:lamp {:on false}}} :lamp :on)))
    (is (not (gs/set-thing-flag? {:objects {:lamp {}}} :lamp :on)))))

;; Current room convenience functions

(deftest set-here-flag-test
  (testing "set-here-flag sets a flag on the current room"
    (let [game-state {:here :kitchen :rooms {:kitchen {}}}
          result (gs/set-here-flag game-state :touch)]
      (is (= {:kitchen {:touch true}} (:rooms result))))))

(deftest unset-here-flag-test
  (testing "unset-here-flag unsets a flag on the current room"
    (let [game-state {:here :kitchen :rooms {:kitchen {:touch true}}}
          result (gs/unset-here-flag game-state :touch)]
      (is (= {:kitchen {:touch false}} (:rooms result))))))

(deftest set-here-flag?-test
  (testing "set-here-flag? returns true when flag is set on current room"
    (is (gs/set-here-flag? {:here :kitchen :rooms {:kitchen {:lit true}}} :lit)))
  (testing "set-here-flag? returns false when flag is not set"
    (is (not (gs/set-here-flag? {:here :kitchen :rooms {:kitchen {:lit false}}} :lit)))
    (is (not (gs/set-here-flag? {:here :kitchen :rooms {:kitchen {}}} :lit)))))

;; Object and content functions

(deftest add-objects-preserves-order-test
  (testing "add-objects assigns :order field based on definition order"
    (let [gs (gs/add-objects {:objects {}}
                             [{:id :first} {:id :second} {:id :third}])]
      (is (= 0 (get-in gs [:objects :first :order])))
      (is (= 1 (get-in gs [:objects :second :order])))
      (is (= 2 (get-in gs [:objects :third :order]))))))

(deftest get-contents-returns-ordered-test
  (testing "get-contents returns objects in definition order"
    (let [gs (gs/add-objects {:objects {}}
                             [{:id :sack :in :kitchen}
                              {:id :bottle :in :kitchen}
                              {:id :water :in :bottle}
                              {:id :knife :in :attic}])
          kitchen-contents (gs/get-contents gs :kitchen)
          bottle-contents (gs/get-contents gs :bottle)]
      ;; Kitchen should have sack before bottle (definition order)
      (is (= [:sack :bottle] kitchen-contents))
      ;; Bottle should have water
      (is (= [:water] bottle-contents)))))

;; State validation tests

(deftest validate-state-test
  (testing "validate-state returns valid for proper game state"
    (let [gs {:rooms {:kitchen {:id :kitchen}}
              :objects {:lamp {:id :lamp}}
              :here :kitchen
              :winner :lamp
              :player :lamp}
          result (gs/validate-state gs)]
      (is (:valid? result))))

  (testing "validate-state catches nil game state"
    (let [result (gs/validate-state nil)]
      (is (not (:valid? result)))
      (is (some #(clojure.string/includes? % "nil") (:errors result)))))

  (testing "validate-state catches missing :rooms"
    (let [gs {:objects {:lamp {}} :here :kitchen}
          result (gs/validate-state gs)]
      (is (not (:valid? result)))
      (is (some #(clojure.string/includes? % ":rooms") (:errors result)))))

  (testing "validate-state catches empty :rooms"
    (let [gs {:rooms {} :objects {:lamp {}} :here :kitchen}
          result (gs/validate-state gs)]
      (is (not (:valid? result)))
      (is (some #(clojure.string/includes? % "not a non-empty map") (:errors result)))))

  (testing "validate-state catches :here not in :rooms"
    (let [gs {:rooms {:kitchen {}} :objects {:lamp {}} :here :nonexistent}
          result (gs/validate-state gs)]
      (is (not (:valid? result)))
      (is (some #(clojure.string/includes? % ":here") (:errors result))))))

(deftest assert-valid-state-test
  (testing "assert-valid-state returns state when valid"
    (let [gs {:rooms {:kitchen {}} :objects {:lamp {}} :here :kitchen :winner :lamp}]
      (is (= gs (gs/assert-valid-state gs)))))

  (testing "assert-valid-state throws when invalid"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Invalid game state"
                          (gs/assert-valid-state nil)))))
