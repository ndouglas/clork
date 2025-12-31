(ns clork.death-test
  "Tests for player death handling (JIGS-UP)."
  (:require [clojure.test :refer :all]
            [clork.death :as death]
            [clork.game-state :as gs]
            [clork.rooms :as rooms]
            [clork.objects :as objects]
            [clork.utils-test :refer [with-captured-output]]))

;;; ---------------------------------------------------------------------------
;;; TEST HELPERS
;;; ---------------------------------------------------------------------------

(defn make-death-test-state
  "Create a game state for death testing."
  []
  (-> (gs/initial-game-state)
      (gs/add-rooms rooms/all-rooms)
      (gs/add-objects objects/all-objects)
      (assoc :here :troll-room)))

;;; ---------------------------------------------------------------------------
;;; KILL-INTERRUPTS TESTS
;;; ---------------------------------------------------------------------------

(deftest kill-interrupts-test
  (testing "kill-interrupts clears all daemons"
    (let [gs (-> (make-death-test-state)
                 (assoc :daemons {:sword {:ticks 3} :lantern {:ticks 100}}))
          result (death/kill-interrupts gs)]
      (is (= {} (:daemons result)))))

  (testing "kill-interrupts turns off match (when it exists)"
    ;; Add a match object to the game state
    (let [gs (-> (make-death-test-state)
                 (assoc-in [:objects :match] {:id :match :desc "match"})
                 (gs/set-thing-flag :match :on))
          result (death/kill-interrupts gs)]
      (is (not (gs/set-thing-flag? result :match :on)))))

  (testing "kill-interrupts handles missing match gracefully"
    ;; Make sure it doesn't error when match doesn't exist
    (let [gs (make-death-test-state)
          result (death/kill-interrupts gs)]
      (is (= {} (:daemons result))))))

;;; ---------------------------------------------------------------------------
;;; RANDOMIZE-OBJECTS TESTS
;;; ---------------------------------------------------------------------------

(deftest randomize-objects-test
  (testing "randomize-objects moves lamp to living room"
    (let [gs (-> (make-death-test-state)
                 (assoc-in [:objects :lamp :in] :adventurer))
          result (death/randomize-objects gs)]
      (is (= :living-room (get-in result [:objects :lamp :in])))))

  (testing "randomize-objects clears sword treasure value"
    (let [gs (-> (make-death-test-state)
                 (assoc-in [:objects :sword :value] 10))
          result (death/randomize-objects gs)]
      (is (= 0 (get-in result [:objects :sword :value])))))

  (testing "randomize-objects scatters held treasures"
    (let [gs (-> (make-death-test-state)
                 (assoc-in [:objects :painting :in] :adventurer)
                 (assoc-in [:objects :painting :value] 4))
          result (death/randomize-objects gs)
          painting-loc (get-in result [:objects :painting :in])]
      ;; Painting should be moved somewhere (not still with adventurer)
      (is (not= :adventurer painting-loc))
      ;; Should be in one of the land rooms
      (is (contains? #{:west-of-house :north-of-house :south-of-house :behind-house
                       :kitchen :living-room :attic :forest-1 :forest-3 :forest-path
                       :clearing :up-a-tree}
                     painting-loc)))))

;;; ---------------------------------------------------------------------------
;;; JIGS-UP TESTS
;;; ---------------------------------------------------------------------------

(deftest jigs-up-first-death-test
  (testing "first death increments death count"
    (let [gs (make-death-test-state)
          ;; Bind *read-input-fn* to avoid prompting
          result (binding [death/*read-input-fn* (constantly "")]
                   (death/jigs-up gs "You have been slain."))]
      (is (= 1 (:deaths result)))))

  (testing "first death deducts 10 points"
    (let [gs (-> (make-death-test-state)
                 (assoc :score 50))
          result (binding [death/*read-input-fn* (constantly "")]
                   (death/jigs-up gs "You have been slain."))]
      (is (= 40 (:score result)))))

  (testing "first death moves player to forest-1"
    (let [gs (make-death-test-state)
          result (binding [death/*read-input-fn* (constantly "")]
                   (death/jigs-up gs "You have been slain."))]
      (is (= :forest-1 (:here result)))))

  (testing "first death shows death message and resurrection message"
    (let [gs (make-death-test-state)
          [output result] (with-captured-output
                            (binding [death/*read-input-fn* (constantly "")]
                              (death/jigs-up gs "You have been slain.")))]
      (is (.contains output "You have been slain."))
      (is (.contains output "****  You have died  ****"))
      (is (.contains output "you probably deserve another chance")))))

(deftest jigs-up-second-death-test
  (testing "second death still allows resurrection"
    (let [gs (-> (make-death-test-state)
                 (assoc :deaths 1))
          result (binding [death/*read-input-fn* (constantly "")]
                   (death/jigs-up gs "Killed again."))]
      (is (= 2 (:deaths result)))
      (is (= :forest-1 (:here result))))))

(deftest jigs-up-third-death-test
  (testing "third death triggers permanent death (finish)"
    (let [gs (-> (make-death-test-state)
                 (assoc :deaths 2))
          result (binding [death/*read-input-fn* (constantly "quit")]
                   (death/jigs-up gs "You are dead forever."))]
      ;; Should have triggered FINISH which sets :quit when "quit" entered
      (is (= true (:quit result)))))

  (testing "third death shows suicidal maniac message"
    (let [gs (-> (make-death-test-state)
                 (assoc :deaths 2))
          [output result] (with-captured-output
                            (binding [death/*read-input-fn* (constantly "quit")]
                              (death/jigs-up gs "You are dead forever.")))]
      (is (.contains output "suicidal maniac")))))

(deftest jigs-up-double-death-test
  (testing "dying while already dead triggers permanent end"
    (let [gs (-> (make-death-test-state)
                 (assoc :dead true))
          result (binding [death/*read-input-fn* (constantly "quit")]
                   (death/jigs-up gs "You died again."))]
      ;; Should have triggered FINISH
      (is (= true (:quit result)))))

  (testing "double death shows talented person message"
    (let [gs (-> (make-death-test-state)
                 (assoc :dead true))
          [output result] (with-captured-output
                            (binding [death/*read-input-fn* (constantly "quit")]
                              (death/jigs-up gs "You died again.")))]
      (is (.contains output "talented person")))))

;;; ---------------------------------------------------------------------------
;;; FINISH TESTS
;;; ---------------------------------------------------------------------------

(deftest finish-quit-test
  (testing "finish with quit response sets :quit flag"
    (let [gs (make-death-test-state)
          result (binding [death/*read-input-fn* (constantly "quit")]
                   (death/finish gs))]
      (is (= true (:quit result))))))

(deftest finish-restart-test
  (testing "finish with restart response sets :restart flag"
    (let [gs (make-death-test-state)
          result (binding [death/*read-input-fn* (constantly "restart")]
                   (death/finish gs))]
      (is (= true (:restart result))))))
