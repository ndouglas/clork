(ns clork.game-state-test
  "Game state tests for Clork."
  (:require [clojure.test :refer :all]
            [clork.game-state :as gs]))

;; Generic flag functions

(deftest set-flag-test
  (testing "set-flag sets a flag on an object"
    (let [game-state {:objects {:obj1 {}}}]
      (is (= {:objects {:obj1 {:takeable true}}}
             (gs/set-flag game-state :objects :obj1 :takeable)))))
  (testing "set-flag sets a flag on a room"
    (let [game-state {:rooms {:room1 {}}}]
      (is (= {:rooms {:room1 {:lit true}}}
             (gs/set-flag game-state :rooms :room1 :lit))))))

(deftest unset-flag-test
  (testing "unset-flag unsets a flag on an object"
    (let [game-state {:objects {:obj1 {:takeable true}}}]
      (is (= {:objects {:obj1 {:takeable false}}}
             (gs/unset-flag game-state :objects :obj1 :takeable)))))
  (testing "unset-flag unsets a flag on a room"
    (let [game-state {:rooms {:room1 {:lit true}}}]
      (is (= {:rooms {:room1 {:lit false}}}
             (gs/unset-flag game-state :rooms :room1 :lit))))))

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
    (let [game-state {:objects {:lamp {}}}]
      (is (= {:objects {:lamp {:on true}}}
             (gs/set-thing-flag game-state :lamp :on)))))
  (testing "set-thing-flag sets a flag on a room"
    (let [game-state {:rooms {:kitchen {}}}]
      (is (= {:rooms {:kitchen {:visited true}}}
             (gs/set-thing-flag game-state :kitchen :visited)))))
  (testing "set-thing-flag throws for unknown thing"
    (let [game-state {:objects {} :rooms {}}]
      (is (thrown? Exception (gs/set-thing-flag game-state :unknown :flag))))))

(deftest unset-thing-flag-test
  (testing "unset-thing-flag unsets a flag on an object"
    (let [game-state {:objects {:lamp {:on true}}}]
      (is (= {:objects {:lamp {:on false}}}
             (gs/unset-thing-flag game-state :lamp :on)))))
  (testing "unset-thing-flag unsets a flag on a room"
    (let [game-state {:rooms {:kitchen {:visited true}}}]
      (is (= {:rooms {:kitchen {:visited false}}}
             (gs/unset-thing-flag game-state :kitchen :visited))))))

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
    (let [game-state {:here :kitchen :rooms {:kitchen {}}}]
      (is (= {:here :kitchen :rooms {:kitchen {:touch true}}}
             (gs/set-here-flag game-state :touch))))))

(deftest unset-here-flag-test
  (testing "unset-here-flag unsets a flag on the current room"
    (let [game-state {:here :kitchen :rooms {:kitchen {:touch true}}}]
      (is (= {:here :kitchen :rooms {:kitchen {:touch false}}}
             (gs/unset-here-flag game-state :touch))))))

(deftest set-here-flag?-test
  (testing "set-here-flag? returns true when flag is set on current room"
    (is (gs/set-here-flag? {:here :kitchen :rooms {:kitchen {:lit true}}} :lit)))
  (testing "set-here-flag? returns false when flag is not set"
    (is (not (gs/set-here-flag? {:here :kitchen :rooms {:kitchen {:lit false}}} :lit)))
    (is (not (gs/set-here-flag? {:here :kitchen :rooms {:kitchen {}}} :lit)))))
