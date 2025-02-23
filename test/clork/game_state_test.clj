(in-ns 'clork.core-test)

(deftest set-obj-flag-test
  (testing "(set-obj-flag game-state obj-id flag) sets a flag on an object"
    (let [game-state {:objects {1 {}}}]
      (is (= {:objects {1 {1 true}}} (set-obj-flag game-state 1 1))))))

(deftest unset-obj-flag-test
  (testing "(unset-obj-flag game-state obj-id flag) unsets a flag on an object"
    (let [game-state {:objects {1 {1 true}}}]
      (is (= {:objects {1 {1 false}}} (unset-obj-flag game-state 1 1))))))

(deftest set-obj-flag?-test
  (testing "(set-obj-flag? game-state obj-id flag) returns true if a flag is set on an object"
    (let [game-state {:objects {1 {1 true}}}]
      (is (set-obj-flag? game-state 1 1)))
    (let [game-state {:objects {1 {1 false}}}]
      (is (not (set-obj-flag? game-state 1 1))))))

(deftest set-room-flag-test
  (testing "(set-room-flag game-state room-id flag) sets a flag on a room"
    (let [game-state {:rooms {1 {}}}]
      (is (= {:rooms {1 {1 true}}} (set-room-flag game-state 1 1))))))

(deftest unset-room-flag-test
  (testing "(unset-room-flag game-state room-id flag) unsets a flag on a room"
    (let [game-state {:rooms {1 {1 true}}}]
      (is (= {:rooms {1 {1 false}}} (unset-room-flag game-state 1 1))))))

(deftest set-room-flag?-test
  (testing "(set-room-flag? game-state room-id flag) returns true if a flag is set on a room"
    (let [game-state {:rooms {1 {1 true}}}]
      (is (set-room-flag? game-state 1 1)))
    (let [game-state {:rooms {1 {1 false}}}]
      (is (not (set-room-flag? game-state 1 1))))))

(deftest set-adv-flag-test
  (testing "(set-adv-flag game-state flag) sets a flag on the adventurer"
    (let [game-state {:adventurer 1 :objects {1 {}}}]
      (is (= {:adventurer 1 :objects {1 {1 true}}} (set-adv-flag game-state 1))))))

(deftest unset-adv-flag-test
  (testing "(unset-adv-flag game-state flag) unsets a flag on the adventurer"
    (let [game-state {:adventurer 1 :objects {1 {1 true}}}]
      (is (= {:adventurer 1 :objects {1 {1 false}}} (unset-adv-flag game-state 1))))))

(deftest set-adv-flag?-test
  (testing "(set-adv-flag? game-state flag) returns true if a flag is set on the adventurer"
    (let [game-state {:adventurer 1 :objects {1 {1 true}}}]
      (is (set-adv-flag? game-state 1)))
    (let [game-state {:adventurer 1 :objects {1 {1 false}}}]
      (is (not (set-adv-flag? game-state 1)))))
    (let [game-state {:adventurer 1 :objects {1 {}}}]
      (is (not (set-adv-flag? game-state 1)))))

(deftest set-here-flag-test
  (testing "(set-here-flag game-state flag) sets a flag on the current room"
    (let [game-state {:here 1 :rooms {1 {}}}]
      (is (= {:here 1 :rooms {1 {1 true}}} (set-here-flag game-state 1)))))
    (let [game-state {:here 1 :rooms {1 {1 true}}}]
      (is (= {:here 1 :rooms {1 {1 true}}} (set-here-flag game-state 1)))))

(deftest unset-here-flag-test
  (testing "(unset-here-flag game-state flag) unsets a flag on the current room"
    (let [game-state {:here 1 :rooms {1 {1 true}}}]
      (is (= {:here 1 :rooms {1 {1 false}}} (unset-here-flag game-state 1)))))
    (let [game-state {:here 1 :rooms {1 {1 false}}}]
      (is (= {:here 1 :rooms {1 {1 false}}} (unset-here-flag game-state 1)))))

(deftest set-here-flag?-test
  (testing "(set-here-flag? game-state flag) returns true if a flag is set on the current room"
    (let [game-state {:here 1 :rooms {1 {1 true}}}]
      (is (set-here-flag? game-state 1)))
    (let [game-state {:here 1 :rooms {1 {1 false}}}]
      (is (not (set-here-flag? game-state 1))))
    (let [game-state {:here 1 :rooms {1 {}}}]
      (is (not (set-here-flag? game-state 1))))))