(ns clork.planner2.speculative
  "Speculative execution for deterministic combat outcomes.

   The game uses a seeded PRNG, which means combat outcomes are deterministic
   given the RNG state. This module exploits that to find action sequences
   that guarantee favorable combat outcomes.

   Strategy:
   1. Save RNG state before risky action
   2. Speculatively execute combat
   3. If outcome is bad, try 'burning' RNG calls with safe actions
   4. Search for action sequence that produces victory

   This is essentially automated save-scumming, but deterministic."
  (:require [clork.random :as rng]
            [clork.planner2.combat :as combat]
            [clork.planner2.observe :as obs]
            [clork.game-state :as gs]
            [clork.combat :as combat-engine]))

;;; ---------------------------------------------------------------------------
;;; SPECULATIVE COMBAT SIMULATION
;;; ---------------------------------------------------------------------------

(defn simulate-combat-outcome
  "Simulate combat and return the outcome without affecting real game state.
   Uses current RNG state.

   Returns {:outcome :win/:death/:timeout, :turns N, :rng-consumed N}."
  [game-state enemy-id]
  (let [rng-before (rng/save-state)
        ;; Run simulation using the combat module's simulate-fight
        winner (:winner game-state)
        weapon (combat-engine/find-weapon game-state winner)
        result (combat/simulate-fight game-state enemy-id weapon 50)
        rng-after (rng/save-state)
        rng-consumed (- (:call-count rng-after) (:call-count rng-before))]
    ;; Restore RNG state (simulation shouldn't affect real state)
    (rng/restore-state! rng-before)
    (assoc result :rng-consumed rng-consumed)))

(defn speculative-combat
  "Execute combat speculatively and return outcome + RNG info.
   Does NOT modify the real RNG state.

   Returns {:outcome :win/:death, :turns N, :final-rng-state RngState}."
  [game-state enemy-id]
  (let [initial-state (rng/save-state)
        result (simulate-combat-outcome game-state enemy-id)]
    ;; RNG is already restored by simulate-combat-outcome
    (assoc result :initial-rng-state initial-state)))

;;; ---------------------------------------------------------------------------
;;; RNG BURNING ACTIONS
;;; ---------------------------------------------------------------------------

(def burn-actions
  "Actions that consume RNG without significant game state changes.
   Each action may consume different amounts of RNG."
  [{:cmd "wait" :typical-rng 0}      ; Usually no RNG
   {:cmd "look" :typical-rng 0}      ; Usually no RNG
   {:cmd "inventory" :typical-rng 0} ; No RNG
   {:cmd "diagnose" :typical-rng 0}  ; No RNG
   {:cmd "verbose" :typical-rng 0}]) ; No RNG (but thief daemon might run!)

(defn simple-burn-sequences
  "Generate simple RNG-burning sequences.
   Mostly relies on thief daemon consuming RNG each turn."
  [max-waits]
  (concat
   ;; Single waits
   (for [n (range 1 (inc max-waits))]
     (vec (repeat n "wait")))
   ;; Alternating wait/look
   (for [n (range 1 (inc (quot max-waits 2)))]
     (vec (interleave (repeat n "wait") (repeat n "look"))))))

;;; ---------------------------------------------------------------------------
;;; WINNING SEQUENCE SEARCH
;;; ---------------------------------------------------------------------------

(defrecord CombatPlan
  [pre-actions      ; Vector of actions to take before combat
   expected-outcome ; :win (we only return winning plans)
   expected-turns   ; Expected number of combat turns
   rng-state        ; RNG state that produces this outcome
   confidence])     ; How certain we are (1.0 = deterministic)

(defn find-winning-combat-plan
  "Search for an action sequence that leads to combat victory.

   Parameters:
   - game-state: Current game state
   - enemy-id: :troll or :thief
   - execute-fn: Function (gs cmd) -> gs to execute commands
   - max-burn-actions: Maximum 'wait' actions to try (default 10)

   Returns CombatPlan or nil if no winning sequence found."
  [game-state enemy-id execute-fn & {:keys [max-burn-actions] :or {max-burn-actions 10}}]
  (let [initial-rng (rng/save-state)]
    ;; First try: combat immediately
    (let [result (speculative-combat game-state enemy-id)]
      (if (= :win (:outcome result))
        ;; Immediate win!
        (->CombatPlan [] :win (:turns result) initial-rng 1.0)

        ;; Try burning RNG with wait sequences
        (loop [sequences (simple-burn-sequences max-burn-actions)]
          (if (empty? sequences)
            nil  ; No winning sequence found

            (let [burn-seq (first sequences)
                  ;; Execute burn sequence speculatively
                  [final-gs _] (rng/with-speculative []
                                 (reduce (fn [gs cmd]
                                           (execute-fn gs cmd))
                                         game-state
                                         burn-seq))
                  ;; Now try combat
                  combat-result (speculative-combat final-gs enemy-id)]

              (if (= :win (:outcome combat-result))
                ;; Found a winning sequence!
                (->CombatPlan burn-seq :win (:turns combat-result)
                              (rng/save-state) 1.0)

                ;; Try next sequence
                (recur (rest sequences))))))))))

(defn find-best-combat-plan
  "Find the best (shortest) winning combat plan.

   Searches multiple burn sequences and returns the one with:
   1. Fewest pre-actions
   2. Fewest combat turns (tie-breaker)

   Returns CombatPlan or nil."
  [game-state enemy-id execute-fn & {:keys [max-burn-actions] :or {max-burn-actions 10}}]
  (let [initial-rng (rng/save-state)
        all-plans (atom [])

        ;; Try immediate combat
        immediate (speculative-combat game-state enemy-id)
        _ (when (= :win (:outcome immediate))
            (swap! all-plans conj
                   (->CombatPlan [] :win (:turns immediate) initial-rng 1.0)))

        ;; Try burn sequences
        _ (doseq [burn-seq (simple-burn-sequences max-burn-actions)]
            (rng/restore-state! initial-rng)
            (let [[final-gs _] (rng/with-speculative []
                                 (reduce (fn [gs cmd]
                                           (execute-fn gs cmd))
                                         game-state
                                         burn-seq))
                  result (speculative-combat final-gs enemy-id)]
              (when (= :win (:outcome result))
                (swap! all-plans conj
                       (->CombatPlan burn-seq :win (:turns result)
                                     (rng/save-state) 1.0)))))

        ;; Restore original RNG state
        _ (rng/restore-state! initial-rng)]

    ;; Return best plan (fewest total actions)
    (when (seq @all-plans)
      (->> @all-plans
           (sort-by (fn [p] [(count (:pre-actions p)) (:expected-turns p)]))
           first))))

;;; ---------------------------------------------------------------------------
;;; COMBAT EXECUTION WITH GUARANTEED OUTCOME
;;; ---------------------------------------------------------------------------

(defn execute-guaranteed-combat
  "Execute combat with a known-winning plan.

   Parameters:
   - game-state: Current state
   - enemy-id: Enemy to fight
   - plan: CombatPlan from find-winning-combat-plan
   - execute-fn: Function (gs cmd) -> gs

   Returns {:success? bool, :game-state gs, :turns N}."
  [game-state enemy-id plan execute-fn]
  (if (nil? plan)
    {:success? false
     :game-state game-state
     :error "No winning plan provided"}

    (let [;; Execute pre-actions
          gs-after-burn (reduce execute-fn game-state (:pre-actions plan))
          ;; Execute combat
          combat-result (combat/execute-combat gs-after-burn enemy-id execute-fn)]
      {:success? (:success? combat-result)
       :game-state (:game-state combat-result)
       :turns (:turns combat-result)
       :pre-actions (:pre-actions plan)
       :events (:events combat-result)})))

;;; ---------------------------------------------------------------------------
;;; COMBAT TIMING ANALYSIS
;;; ---------------------------------------------------------------------------

(defn analyze-combat-timing
  "Analyze how combat outcome varies with RNG state.
   Useful for understanding when to engage.

   Returns map with outcome distribution over next N RNG states."
  [game-state enemy-id execute-fn num-samples]
  (let [initial-rng (rng/save-state)
        results (atom {:wins 0 :deaths 0 :timeouts 0 :by-burn []})]

    (doseq [burn-count (range num-samples)]
      (rng/restore-state! initial-rng)
      ;; Burn RNG
      (rng/advance-rng! burn-count)
      ;; Simulate combat
      (let [outcome (:outcome (speculative-combat game-state enemy-id))]
        (swap! results update outcome (fnil inc 0))
        (swap! results update :by-burn conj
               {:burn-count burn-count :outcome outcome})))

    ;; Restore original state
    (rng/restore-state! initial-rng)

    (let [r @results
          ;; Count all outcomes including errors
          total (count (:by-burn r))
          wins (:wins r 0)
          deaths (:deaths r 0)
          timeouts (:timeouts r 0)
          errors (:errors r 0)]
      {:total-samples total
       :win-rate (if (pos? total) (double (/ wins total)) 0.0)
       :death-rate (if (pos? total) (double (/ deaths total)) 0.0)
       :wins wins
       :deaths deaths
       :timeouts timeouts
       :errors errors
       :first-win (first (keep-indexed
                          (fn [i {:keys [outcome]}]
                            (when (= outcome :win) i))
                          (:by-burn r)))})))

;;; ---------------------------------------------------------------------------
;;; FORMAT OUTPUT
;;; ---------------------------------------------------------------------------

(defn format-combat-plan
  "Format a CombatPlan for display."
  [plan enemy-id]
  (if plan
    (str "=== Combat Plan vs " (name enemy-id) " ===\n"
         "Pre-actions: " (if (empty? (:pre-actions plan))
                          "none (fight immediately)"
                          (pr-str (:pre-actions plan))) "\n"
         "Expected outcome: " (name (:expected-outcome plan)) "\n"
         "Expected turns: " (:expected-turns plan) "\n"
         "Confidence: " (format "%.0f%%" (* 100 (:confidence plan))) "\n")
    "No winning plan found.\n"))

(defn print-combat-plan
  "Print a combat plan."
  [plan enemy-id]
  (println (format-combat-plan plan enemy-id)))

(defn format-timing-analysis
  "Format timing analysis for display."
  [analysis enemy-id]
  (str "=== Combat Timing Analysis vs " (name enemy-id) " ===\n"
       "Samples: " (:total-samples analysis) "\n"
       "Win rate: " (format "%.1f%%" (* 100 (:win-rate analysis))) "\n"
       "Death rate: " (format "%.1f%%" (* 100 (:death-rate analysis))) "\n"
       "First winning RNG offset: " (or (:first-win analysis) "none found") "\n"))

(defn print-timing-analysis
  "Print timing analysis."
  [analysis enemy-id]
  (println (format-timing-analysis analysis enemy-id)))
