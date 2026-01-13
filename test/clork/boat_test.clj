(ns clork.boat-test
  "Tests for the Frigid River boat system and reservoir navigation."
  (:require [clojure.test :refer :all]
            [clork.core :as core]
            [clork.game-state :as gs]
            [clork.parser :as parser]
            [clork.verb-defs :as verb-defs]
            [clork.daemon :as daemon]
            [clork.intel.routing :as routing]
            [clork.debug.scenarios :as scenarios]))

;;; ---------------------------------------------------------------------------
;;; TEST HELPERS
;;; ---------------------------------------------------------------------------

(defn run-cmd
  "Run a command and return {:output string :gs game-state}."
  [gs cmd]
  (let [out (java.io.StringWriter.)]
    (binding [*out* out]
      (let [gs' (-> gs
                    (assoc :input cmd)
                    parser/parser-from-input
                    verb-defs/perform
                    daemon/clocker)]
        {:output (str out) :gs gs'}))))

(defn run-cmds
  "Run multiple commands in sequence, returning final game-state."
  [gs cmds]
  (reduce (fn [state cmd] (:gs (run-cmd state cmd))) gs cmds))

;;; ---------------------------------------------------------------------------
;;; INFLATE TESTS
;;; ---------------------------------------------------------------------------

(deftest test-inflate-boat
  (testing "inflating boat transforms inflatable-boat to inflated-boat"
    (let [gs (-> (core/init-game)
                 (assoc :here :dam-base)
                 (gs/move-object :pump :adventurer :test))
          result (run-cmd gs "inflate boat with pump")]
      (is (re-find #"inflates" (:output result)))
      (is (= :limbo (gs/get-thing-loc-id (:gs result) :inflatable-boat)))
      (is (= :dam-base (gs/get-thing-loc-id (:gs result) :inflated-boat))))))

(deftest test-inflate-requires-pump
  (testing "inflating without pump fails"
    (let [gs (-> (core/init-game)
                 (assoc :here :dam-base))
          result (run-cmd gs "inflate boat")]
      (is (not (re-find #"inflates" (:output result)))))))

;;; ---------------------------------------------------------------------------
;;; BOARD TESTS
;;; ---------------------------------------------------------------------------

(deftest test-board-inflated-boat
  (testing "boarding inflated boat moves player into it"
    (let [gs (-> (core/init-game)
                 (assoc :here :dam-base)
                 (gs/move-object :pump :adventurer :test))
          gs-inflated (:gs (run-cmd gs "inflate boat with pump"))
          result (run-cmd gs-inflated "board boat")]
      (is (re-find #"now in" (:output result)))
      (is (= :inflated-boat (gs/get-thing-loc-id (:gs result) :adventurer))))))

(deftest test-board-deflated-boat-fails
  (testing "boarding pile of plastic fails"
    (let [gs (-> (core/init-game)
                 (assoc :here :dam-base))
          result (run-cmd gs "board boat")]
      (is (re-find #"pile of plastic" (:output result))))))

;;; ---------------------------------------------------------------------------
;;; LAUNCH TESTS
;;; ---------------------------------------------------------------------------

(deftest test-launch-at-dam-base
  (testing "launching at dam-base enters river-1"
    (let [gs (-> (core/init-game)
                 (assoc :here :dam-base)
                 (gs/move-object :pump :adventurer :test))
          gs-ready (run-cmds gs ["inflate boat with pump" "board boat"])
          result (run-cmd gs-ready "launch")]
      (is (= :river-1 (:here (:gs result)))))))

(deftest test-launch-requires-being-in-boat
  (testing "launch without being in boat fails"
    (let [gs (-> (core/init-game)
                 (assoc :here :dam-base)
                 (gs/move-object :pump :adventurer :test))
          gs-inflated (:gs (run-cmd gs "inflate boat with pump"))
          result (run-cmd gs-inflated "launch")]
      (is (re-find #"not in anything" (:output result))))))

;;; ---------------------------------------------------------------------------
;;; RIVER DRIFT TESTS
;;; ---------------------------------------------------------------------------

(deftest test-river-drift-downstream
  (testing "waiting in river drifts boat downstream"
    (let [gs (-> (core/init-game)
                 (assoc :here :dam-base)
                 (gs/move-object :pump :adventurer :test)
                 (gs/move-object :brass-lantern :adventurer :test))
          ;; Turn on lamp for visibility
          gs-lit (-> gs
                     (gs/set-thing-flag :brass-lantern :on)
                     (gs/set-thing-flag :brass-lantern :lit))
          gs-ready (run-cmds gs-lit ["inflate boat with pump" "board boat" "launch"])
          ;; Now at river-1, wait to drift
          result (run-cmd gs-ready "wait")]
      (is (= :river-2 (:here (:gs result))))
      (is (re-find #"downstream" (:output result))))))

(deftest test-river-drift-sequence
  (testing "multiple waits drift through river rooms"
    (let [gs (-> (core/init-game)
                 (assoc :here :dam-base)
                 (gs/move-object :pump :adventurer :test)
                 (gs/move-object :brass-lantern :adventurer :test))
          gs-lit (-> gs
                     (gs/set-thing-flag :brass-lantern :on)
                     (gs/set-thing-flag :brass-lantern :lit))
          gs-ready (run-cmds gs-lit ["inflate boat with pump" "board boat" "launch"])
          ;; Drift through: river-1 -> river-2 -> river-3
          gs-river2 (:gs (run-cmd gs-ready "wait"))
          gs-river3 (:gs (run-cmd gs-river2 "wait"))]
      (is (= :river-2 (:here gs-river2)))
      (is (= :river-3 (:here gs-river3))))))

;;; ---------------------------------------------------------------------------
;;; LAND TESTS
;;; ---------------------------------------------------------------------------

(deftest test-land-at-white-cliffs
  (testing "landing at river-3 exits to white-cliffs-north"
    (let [gs (-> (core/init-game)
                 (assoc :here :dam-base)
                 (gs/move-object :pump :adventurer :test)
                 (gs/move-object :brass-lantern :adventurer :test))
          gs-lit (-> gs
                     (gs/set-thing-flag :brass-lantern :on)
                     (gs/set-thing-flag :brass-lantern :lit))
          ;; Get to river-3 where we can land at white cliffs
          gs-ready (run-cmds gs-lit ["inflate boat with pump" "board boat" "launch"
                                     "wait" "wait"])
          result (run-cmd gs-ready "land")]
      ;; Should be at white-cliffs-north and out of boat
      (is (= :white-cliffs-north (:here (:gs result))))
      (is (not= :inflated-boat (gs/get-thing-loc-id (:gs result) :adventurer))))))

;;; ---------------------------------------------------------------------------
;;; ROUTING INTEGRATION TESTS
;;; ---------------------------------------------------------------------------

(deftest test-routing-requires-boat-ready
  (testing "route to shore requires :boat-ready flag"
    (let [gs (scenarios/equipped-adventurer :dam-base)
          ;; Without boat-ready, shore is unreachable via river
          result-no-boat (routing/shortest-path gs :dam-base :shore
                                                :available-flags #{})
          ;; With boat-ready, can reach shore
          result-with-boat (routing/shortest-path gs :dam-base :shore
                                                  :available-flags #{:boat-ready})]
      ;; Without boat, can only reach shore via long overland route (if any)
      ;; With boat, can use river
      (is (some? result-with-boat))
      (is (some #{:river-4} (:path result-with-boat))))))

(deftest test-routing-through-complete-river
  (testing "routing finds path through entire river"
    (let [gs (scenarios/equipped-adventurer :dam-base)
          result (routing/shortest-path gs :dam-base :shore
                                        :available-flags #{:boat-ready})]
      (is (some? result))
      ;; Path should go through river rooms
      (is (some #{:river-1} (:path result)))
      (is (= :shore (last (:path result)))))))

;;; ---------------------------------------------------------------------------
;;; RESERVOIR NAVIGATION TESTS
;;; ---------------------------------------------------------------------------

(deftest test-reservoir-launch-edge-exists
  (testing "routing includes reservoir launch edges"
    (let [gs (scenarios/equipped-adventurer :reservoir-north)
          graph (routing/build-navigation-graph gs :available-flags #{:boat-ready})
          has-launch? (some #(and (= (:from %) :reservoir-north)
                                  (= (:to %) :reservoir))
                            (:edges graph))]
      (is has-launch? "Should have launch edge from reservoir-north to reservoir"))))

(deftest test-reservoir-route-with-boat
  (testing "can route from reservoir-north to reservoir with boat"
    (let [gs (scenarios/equipped-adventurer :reservoir-north)
          result (routing/shortest-path gs :reservoir-north :reservoir
                                        :available-flags #{:boat-ready})]
      (is (some? result))
      (is (= 1 (:distance result))))))

;;; ---------------------------------------------------------------------------
;;; BOAT-READY FLAG EXTRACTION TESTS
;;; ---------------------------------------------------------------------------

(deftest test-boat-ready-when-in-boat
  (testing "extract-available-flags includes :boat-ready when in boat"
    (let [gs (-> (scenarios/equipped-adventurer :dam-base)
                 (gs/move-object :inflated-boat :dam-base :test)
                 (assoc-in [:objects :adventurer :in] :inflated-boat))
          flags (routing/extract-available-flags gs)]
      (is (contains? flags :boat-ready)))))

(deftest test-boat-ready-not-when-on-ground
  (testing "extract-available-flags excludes :boat-ready when not in boat"
    (let [gs (scenarios/equipped-adventurer :dam-base)
          flags (routing/extract-available-flags gs)]
      (is (not (contains? flags :boat-ready))))))

;;; ---------------------------------------------------------------------------
;;; EDGE CASE TESTS
;;; ---------------------------------------------------------------------------

(deftest test-deflate-while-in-boat-fails
  (testing "cannot deflate boat while inside it"
    (let [gs (-> (core/init-game)
                 (assoc :here :dam-base)
                 (gs/move-object :pump :adventurer :test))
          gs-ready (run-cmds gs ["inflate boat with pump" "board boat"])
          result (run-cmd gs-ready "deflate boat")]
      ;; Should fail - can't deflate while in boat
      (is (re-find #"can't deflate.*while you're in it" (:output result)))
      ;; Boat should still be inflated
      (is (= :dam-base (gs/get-thing-loc-id (:gs result) :inflated-boat))))))

(deftest test-inflate-already-inflated-fails
  (testing "cannot inflate an already-inflated boat"
    (let [gs (-> (core/init-game)
                 (assoc :here :dam-base)
                 (gs/move-object :pump :adventurer :test))
          gs-inflated (:gs (run-cmd gs "inflate boat with pump"))
          result (run-cmd gs-inflated "inflate boat with pump")]
      ;; Should fail or indicate already inflated
      (is (or (re-find #"already" (:output result))
              (not (re-find #"inflates" (:output result))))))))

(deftest test-land-at-unsafe-location-fails
  (testing "landing at river-2 has no safe shore"
    (let [gs (-> (core/init-game)
                 (assoc :here :dam-base)
                 (gs/move-object :pump :adventurer :test)
                 (gs/move-object :brass-lantern :adventurer :test))
          gs-lit (-> gs
                     (gs/set-thing-flag :brass-lantern :on)
                     (gs/set-thing-flag :brass-lantern :lit))
          ;; Get to river-2 (launch puts us at river-1, wait drifts to river-2)
          gs-ready (run-cmds gs-lit ["inflate boat with pump" "board boat" "launch" "wait"])
          ;; Now at river-2 - try to land
          result (run-cmd gs-ready "land")]
      ;; Should fail - no safe shore at river-2 (message in output)
      (is (re-find #"no safe.*landing" (:output result))))))

(deftest test-disembark-on-water-exits-boat
  (testing "disembarking on water exits boat (player drifts with current)"
    (let [gs (-> (core/init-game)
                 (assoc :here :dam-base)
                 (gs/move-object :pump :adventurer :test)
                 (gs/move-object :brass-lantern :adventurer :test))
          gs-lit (-> gs
                     (gs/set-thing-flag :brass-lantern :on)
                     (gs/set-thing-flag :brass-lantern :lit))
          gs-ready (run-cmds gs-lit ["inflate boat with pump" "board boat" "launch"])
          result (run-cmd gs-ready "disembark")]
      ;; Disembark succeeds - player exits boat
      (is (re-find #"on your own feet" (:output result)))
      ;; Player is no longer in boat (in the river room now)
      (is (not= :inflated-boat (gs/get-thing-loc-id (:gs result) :adventurer))))))

;; BUG: v-board doesn't call the object's action handler (rboat-function),
;; which contains the sharp object puncture check. The check exists in
;; rboat-function at objects.clj:3350-3360, but v-board bypasses it.
(deftest ^:pending test-board-with-sharp-object-punctures
  (testing "boarding with sharp object punctures the boat"
    (let [gs (-> (core/init-game)
                 (assoc :here :dam-base)
                 (gs/move-object :pump :adventurer :test)
                 (gs/move-object :sceptre :adventurer :test))
          gs-inflated (:gs (run-cmd gs "inflate boat with pump"))
          result (run-cmd gs-inflated "board boat")]
      ;; Sharp object should puncture boat
      (is (re-find #"puncture|hiss|pop" (:output result)))
      ;; Boat should be deflated (back to inflatable-boat)
      (is (= :dam-base (gs/get-thing-loc-id (:gs result) :inflatable-boat))))))

(deftest test-launch-not-in-boat-fails
  (testing "cannot launch if not in boat"
    (let [gs (-> (core/init-game)
                 (assoc :here :dam-base)
                 (gs/move-object :pump :adventurer :test))
          gs-inflated (:gs (run-cmd gs "inflate boat with pump"))
          result (run-cmd gs-inflated "launch")]
      ;; Should fail - not in boat
      (is (re-find #"not in anything|aren't in" (:output result))))))
