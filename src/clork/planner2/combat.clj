(ns clork.planner2.combat
  "Combat simulation and management for the speedrun planner.

   Provides:
   - Combat test harness (simulate N fights, gather statistics)
   - Combat wrapper with recovery (weapon pickup, death detection)
   - Pre-combat invariant checking

   Note: Player strength varies with score:
   - 0 points: strength 2
   - 350 points: strength 7
   This significantly affects combat outcomes."
  (:require [clork.planner2.observe :as obs]
            [clork.game-state :as gs]
            [clork.verbs-health :as health]
            [clork.combat :as combat-engine]
            [clork.random :as random]))

;;; ---------------------------------------------------------------------------
;;; COMBAT STATISTICS
;;; ---------------------------------------------------------------------------

(defrecord CombatStats
  [total-fights
   wins
   deaths
   avg-turns
   min-turns
   max-turns
   weapon-drops
   wounds-taken
   player-strength])

(defn make-combat-stats
  "Create initial combat statistics."
  []
  (->CombatStats 0 0 0 0.0 Integer/MAX_VALUE 0 0 0 0))

(defn update-stats
  "Update statistics with results from a fight."
  [stats {:keys [outcome turns weapon-dropped? wounds player-strength]}]
  (let [n (inc (:total-fights stats))]
    (-> stats
        (assoc :total-fights n)
        (update :wins + (if (= outcome :win) 1 0))
        (update :deaths + (if (= outcome :death) 1 0))
        (assoc :avg-turns (/ (+ (* (:avg-turns stats) (:total-fights stats)) turns) n))
        (update :min-turns min turns)
        (update :max-turns max turns)
        (update :weapon-drops + (if weapon-dropped? 1 0))
        (update :wounds-taken + wounds)
        (assoc :player-strength player-strength))))

;;; ---------------------------------------------------------------------------
;;; SINGLE COMBAT SIMULATION
;;; ---------------------------------------------------------------------------

(defn simulate-single-attack
  "Simulate a single attack exchange (player attacks, villain counter-attacks).
   Returns {:game-state gs :player-result r1 :villain-result r2}."
  [game-state villain-id weapon-id]
  (let [;; Get villain entry
        villain-entry (get combat-engine/villains-registry villain-id)
        winner (:winner game-state)

        ;; Player's attack
        att (max 1 (health/fight-strength game-state))
        def (combat-engine/villain-strength game-state villain-entry weapon-id)
        villain-weapon (combat-engine/find-weapon game-state villain-id)

        ;; Can't attack if villain is dead (strength = 0), but unconscious (negative) is ok
        _ (when (zero? def) (throw (ex-info "Villain already dead" {:def def})))

        ;; Roll player's attack
        [table _] (combat-engine/select-result-table def att)
        player-result (if (or (nil? villain-weapon) (neg? def))
                        ;; Auto-kill unarmed/unconscious
                        combat-engine/killed
                        (combat-engine/roll-combat-result table))

        ;; Apply result to villain
        new-villain-strength (cond
                               (#{combat-engine/missed combat-engine/hesitate} player-result) def
                               (= combat-engine/unconscious player-result) (- def)
                               (#{combat-engine/killed combat-engine/sitting-duck} player-result) 0
                               (= combat-engine/light-wound player-result) (max 0 (- def 1))
                               (= combat-engine/serious-wound player-result) (max 0 (- def 2))
                               :else def)

        gs-after-player (-> game-state
                            (assoc-in [:objects villain-id :strength] new-villain-strength)
                            (cond-> (= combat-engine/lose-weapon player-result)
                              (combat-engine/drop-weapon villain-id villain-weapon)))

        ;; Check if villain died
        villain-dead? (zero? new-villain-strength)]

    (if villain-dead?
      {:game-state (assoc-in gs-after-player [:objects villain-id :in] :limbo)
       :player-result player-result
       :villain-result nil
       :villain-dead true
       :player-dead false}

      ;; Villain counter-attacks
      (let [;; Villain's attack
            v-att (combat-engine/villain-strength gs-after-player villain-entry nil)
            v-def (health/fight-strength gs-after-player)
            player-weapon (combat-engine/find-weapon gs-after-player winner)

            ;; Roll villain's attack (if not staggered)
            villain-staggered? (gs/set-thing-flag? gs-after-player villain-id :staggered)
            villain-result (if villain-staggered?
                             nil
                             (let [[v-table _] (combat-engine/select-result-table v-def v-att)]
                               (combat-engine/roll-combat-result v-table)))

            ;; Apply result to player
            new-player-strength (if villain-result
                                  (cond
                                    (#{combat-engine/missed combat-engine/hesitate} villain-result) v-def
                                    (= combat-engine/unconscious villain-result) v-def ; knocked out but not dead
                                    (#{combat-engine/killed combat-engine/sitting-duck} villain-result) 0
                                    (= combat-engine/light-wound villain-result) (max 0 (- v-def 1))
                                    (= combat-engine/serious-wound villain-result) (max 0 (- v-def 2))
                                    :else v-def)
                                  v-def)

            gs-after-villain (-> gs-after-player
                                 (cond-> villain-result
                                   (-> (update-in [:objects winner :strength]
                                                  (fn [s] (+ (or s 0) (- new-player-strength v-def))))
                                       (cond-> (= combat-engine/lose-weapon villain-result)
                                         (combat-engine/drop-weapon winner player-weapon)))))]

        {:game-state gs-after-villain
         :player-result player-result
         :villain-result villain-result
         :villain-dead false
         :player-dead (<= new-player-strength 0)
         :weapon-dropped (= combat-engine/lose-weapon villain-result)}))))

(defn simulate-fight
  "Simulate a full fight until one combatant dies or max turns reached.
   Returns {:outcome :win/:death/:timeout :turns N :weapon-drops N :wounds N}."
  [game-state villain-id weapon-id max-turns]
  (loop [gs game-state
         turns 0
         weapon-drops 0
         wounds 0]
    (if (>= turns max-turns)
      {:outcome :timeout
       :turns turns
       :weapon-drops weapon-drops
       :wounds wounds
       :player-strength (health/fight-strength game-state false)}

      (let [;; Check if we still have weapon
            winner (:winner gs)
            current-weapon (combat-engine/find-weapon gs winner)
            gs-with-weapon (if (and (nil? current-weapon) weapon-id)
                             ;; Try to pick up dropped weapon
                             (if (= (:here gs) (get-in gs [:objects weapon-id :in]))
                               (assoc-in gs [:objects weapon-id :in] winner)
                               gs)
                             gs)

            result (try
                     (simulate-single-attack gs-with-weapon villain-id
                                             (or current-weapon weapon-id))
                     (catch Exception e
                       {:error e}))]

        (cond
          (:error result)
          {:outcome :error
           :turns turns
           :weapon-drops weapon-drops
           :wounds wounds
           :error (:error result)
           :player-strength (health/fight-strength game-state false)}

          (:villain-dead result)
          {:outcome :win
           :turns (inc turns)
           :weapon-drops (+ weapon-drops (if (:weapon-dropped result) 1 0))
           :wounds wounds
           :player-strength (health/fight-strength game-state false)}

          (:player-dead result)
          {:outcome :death
           :turns (inc turns)
           :weapon-drops (+ weapon-drops (if (:weapon-dropped result) 1 0))
           :wounds (inc wounds)
           :player-strength (health/fight-strength game-state false)}

          :else
          (recur (:game-state result)
                 (inc turns)
                 (+ weapon-drops (if (:weapon-dropped result) 1 0))
                 (+ wounds (if (#{combat-engine/light-wound combat-engine/serious-wound}
                                (:villain-result result))
                             1 0))))))))

;;; ---------------------------------------------------------------------------
;;; COMBAT TEST HARNESS
;;; ---------------------------------------------------------------------------

(defn run-combat-simulation
  "Run N simulated fights and gather statistics.

   Parameters:
   - game-state: Initial game state (affects player strength via score)
   - villain-id: :troll or :thief
   - n: Number of fights to simulate
   - max-turns: Max turns per fight (default 50)

   Returns map with statistics."
  [game-state villain-id n & {:keys [max-turns] :or {max-turns 50}}]
  (let [;; Ensure villain is alive and in correct location
        villain-room (case villain-id
                       :troll :troll-room
                       :thief nil ; Thief can be anywhere
                       nil)
        weapon-id :sword

        ;; Setup initial state
        setup-state (-> game-state
                        (assoc :here (or villain-room (:here game-state)))
                        (assoc-in [:objects villain-id :strength] 2) ; Reset villain
                        (assoc-in [:objects villain-id :in] (or villain-room (:here game-state)))
                        (assoc-in [:objects weapon-id :in] (:winner game-state)))]

    ;; Run N fights
    (reduce
     (fn [stats _]
       (let [;; Reset villain for each fight
             fresh-state (-> setup-state
                             (assoc-in [:objects villain-id :strength] 2)
                             (assoc-in [:objects villain-id :in] (or villain-room (:here setup-state)))
                             (assoc-in [:objects weapon-id :in] (:winner setup-state))
                             (assoc-in [:objects (:winner setup-state) :strength] 0)) ; Clear wounds
             result (simulate-fight fresh-state villain-id weapon-id max-turns)]
         (update-stats stats result)))
     (make-combat-stats)
     (range n))))

(defn print-combat-stats
  "Print combat simulation statistics."
  [stats villain-id]
  (let [win-rate (if (pos? (:total-fights stats))
                   (* 100.0 (/ (:wins stats) (:total-fights stats)))
                   0.0)
        death-rate (if (pos? (:total-fights stats))
                     (* 100.0 (/ (:deaths stats) (:total-fights stats)))
                     0.0)]
    (println "=== Combat Simulation Results ===")
    (println "Villain:" (name villain-id))
    (println "Player strength:" (:player-strength stats))
    (println "Total fights:" (:total-fights stats))
    (println (format "Win rate: %.1f%%" win-rate))
    (println (format "Death rate: %.1f%%" death-rate))
    (println "Avg turns:" (format "%.1f" (:avg-turns stats)))
    (println "Min/Max turns:" (:min-turns stats) "/" (:max-turns stats))
    (println "Weapon drops:" (:weapon-drops stats))
    (println "Wounds taken:" (:wounds-taken stats))))

;;; ---------------------------------------------------------------------------
;;; COMBAT WRAPPER
;;; ---------------------------------------------------------------------------

(defn check-combat-ready
  "Check if player is ready for combat.
   Returns {:ready? bool :issues [...]} with list of problems."
  [game-state villain-id]
  (let [winner (:winner game-state)
        issues []
        weapon (combat-engine/find-weapon game-state winner)
        has-light? (or (obs/lit? game-state) (obs/lantern-on? game-state))]
    {:ready? (and weapon has-light?)
     :issues (cond-> issues
               (not weapon) (conj :no-weapon)
               (not has-light?) (conj :no-light))
     :weapon weapon
     :player-strength (health/fight-strength game-state false)}))

(defn estimate-combat-risk
  "Estimate risk level for a combat.
   Returns :low, :medium, :high, or :very-high."
  [game-state villain-id]
  (let [player-str (health/fight-strength game-state false)]
    (case villain-id
      :troll (cond
               (>= player-str 5) :low
               (>= player-str 4) :medium
               (>= player-str 3) :high
               :else :very-high)
      :thief (cond
               (>= player-str 6) :medium
               (>= player-str 4) :high
               :else :very-high)
      :medium)))

;;; ---------------------------------------------------------------------------
;;; COMBAT OUTCOME TRACKING
;;; ---------------------------------------------------------------------------

(defn analyze-combat-outcome
  "Analyze the outcome of a real combat.
   Call after combat to determine what happened."
  [pre-combat-state post-combat-state villain-id]
  (let [winner (:winner post-combat-state)
        pre-weapon (combat-engine/find-weapon pre-combat-state winner)
        post-weapon (combat-engine/find-weapon post-combat-state winner)
        villain-dead? (or (= :limbo (get-in post-combat-state [:objects villain-id :in]))
                          (not (obs/object-visible? post-combat-state villain-id)))
        player-dead? (not (obs/player-alive? post-combat-state))]
    {:outcome (cond
                player-dead? :death
                villain-dead? :win
                :else :ongoing)
     :weapon-lost? (and pre-weapon (not post-weapon))
     :weapon-location (when (and pre-weapon (not post-weapon))
                        (get-in post-combat-state [:objects pre-weapon :in]))
     :player-wounds (get-in post-combat-state [:objects winner :strength] 0)
     :villain-strength (get-in post-combat-state [:objects villain-id :strength])}))

;;; ---------------------------------------------------------------------------
;;; COMBAT WRAPPER - REAL EXECUTION
;;; ---------------------------------------------------------------------------

(defrecord CombatResult
  [success?        ; Boolean - combat completed successfully (villain dead)
   game-state      ; Final game state after combat
   outcome         ; :win, :death, :aborted, :error
   turns           ; Number of combat turns taken
   weapon-pickups  ; Number of times weapon was retrieved
   events])        ; Log of combat events

(defn make-combat-result
  "Create a combat result."
  [success? game-state outcome turns weapon-pickups events]
  (->CombatResult success? game-state outcome turns weapon-pickups (vec events)))

(defn execute-combat
  "Execute real combat against a villain.

   This is the main combat wrapper that:
   1. Verifies combat prerequisites
   2. Executes combat until villain dies or player dies
   3. Handles weapon drops (picks weapon back up)
   4. Returns structured result

   Note: Player death is a RUN-ENDER - there is no recovery.

   Parameters:
   - game-state: Current game state
   - villain-id: :troll or :thief
   - execute-fn: Function (gs command) -> gs that executes a game command

   Returns CombatResult with :success? indicating win."
  [game-state villain-id execute-fn]
  (let [winner (:winner game-state)
        ready-check (check-combat-ready game-state villain-id)]
    (if-not (:ready? ready-check)
      ;; Not ready for combat
      (make-combat-result
       false game-state :aborted 0 0
       [(str "Combat aborted: " (pr-str (:issues ready-check)))])

      ;; Execute combat loop
      (loop [gs game-state
             turns 0
             weapon-pickups 0
             events []]
        (let [;; Check current state
              villain-loc (get-in gs [:objects villain-id :in])
              villain-strength (get-in gs [:objects villain-id :strength] 0)
              villain-dead? (or (= villain-loc :limbo)
                                (and (some? villain-strength)
                                     (<= villain-strength 0)))
              player-dead? (not (obs/player-alive? gs))]

          (cond
            ;; Player died - RUN OVER
            player-dead?
            (make-combat-result
             false gs :death turns weapon-pickups
             (conj events "Player died!"))

            ;; Villain dead - SUCCESS
            villain-dead?
            (make-combat-result
             true gs :win turns weapon-pickups
             (conj events (str "Defeated " (name villain-id) " in " turns " turns")))

            ;; Safety limit
            (> turns 50)
            (make-combat-result
             false gs :timeout turns weapon-pickups
             (conj events "Combat timeout after 50 turns"))

            :else
            ;; Check if we lost our weapon and need to pick it up
            (let [current-weapon (combat-engine/find-weapon gs winner)
                  weapon-in-room? (fn [w] (= (get-in gs [:objects w :in]) (:here gs)))
                  need-weapon-pickup? (and (nil? current-weapon)
                                           (or (weapon-in-room? :sword)
                                               (weapon-in-room? :knife)
                                               (weapon-in-room? :stiletto)))
                  dropped-weapon (when need-weapon-pickup?
                                   (cond
                                     (weapon-in-room? :sword) :sword
                                     (weapon-in-room? :knife) :knife
                                     (weapon-in-room? :stiletto) :stiletto))]

              (if need-weapon-pickup?
                ;; Pick up weapon first
                (let [new-gs (execute-fn gs (str "take " (name dropped-weapon)))]
                  (recur new-gs turns (inc weapon-pickups)
                         (conj events (str "Picked up " (name dropped-weapon)))))

                ;; Attack the villain
                (let [attack-cmd (str "attack " (name villain-id))
                      new-gs (execute-fn gs attack-cmd)]
                  (recur new-gs (inc turns) weapon-pickups
                         (conj events (str "Turn " (inc turns) ": " attack-cmd))))))))))))

(defn format-combat-result
  "Format a CombatResult for display."
  [result villain-id]
  (let [lines [(str "=== Combat Result vs " (name villain-id) " ===")
               (str "Outcome: " (name (:outcome result)))
               (str "Turns: " (:turns result))
               (str "Weapon pickups: " (:weapon-pickups result))
               ""
               "Events:"]]
    (str (clojure.string/join
          "\n"
          (concat lines
                  (for [e (:events result)]
                    (str "  " e))))
         "\n")))

(defn print-combat-result
  "Print a combat result to stdout."
  [result villain-id]
  (println (format-combat-result result villain-id)))

;;; ---------------------------------------------------------------------------
;;; RNG MANIPULATION FOR GUARANTEED WINS
;;; ---------------------------------------------------------------------------
;;; For speedruns, we need deterministic combat success.
;;; This section provides functions to find RNG offsets that produce wins.

(defn find-winning-rng-offset
  "Find an RNG offset that produces a combat win.

   Searches through RNG advances (0 to max-offset) to find one that
   results in the player winning combat without dying.

   Parameters:
   - game-state: Current game state
   - villain-id: :troll or :thief
   - max-offset: Maximum RNG advance to try (default 100)
   - max-turns: Maximum turns per simulated fight (default 20)

   Returns {:offset n :turns t :weapon-drops w} on success, nil if no win found."
  [game-state villain-id & {:keys [max-offset max-turns]
                             :or {max-offset 100 max-turns 20}}]
  (let [saved-state (random/save-state)
        winner (:winner game-state)
        weapon-id (combat-engine/find-weapon game-state winner)
        ;; Setup clean fight state
        villain-room (case villain-id
                       :troll :troll-room
                       :thief nil
                       :cyclops :cyclops-room
                       nil)
        setup-state (-> game-state
                        (assoc-in [:objects villain-id :strength] 2)
                        (assoc-in [:objects winner :strength] 0))]
    (try
      (loop [offset 0]
        (when (< offset max-offset)
          ;; Restore to base state and advance by offset
          (random/restore-state! saved-state)
          (random/advance-rng! offset)

          ;; Simulate the fight
          (let [result (simulate-fight setup-state villain-id weapon-id max-turns)]
            (if (= :win (:outcome result))
              {:offset offset
               :turns (:turns result)
               :weapon-drops (:weapon-drops result)}
              (recur (inc offset))))))
      (finally
        ;; Always restore original RNG state
        (random/restore-state! saved-state)))))

(defn prepare-winning-combat!
  "Advance RNG to a position where combat will succeed.

   Call this IMMEDIATELY before starting combat.
   Returns the winning offset info, or nil if no win found.

   IMPORTANT: After calling this, you must execute combat immediately
   without any other RNG-consuming operations in between."
  [game-state villain-id & {:keys [max-offset] :or {max-offset 100}}]
  (if-let [result (find-winning-rng-offset game-state villain-id
                                            :max-offset max-offset)]
    (do
      (random/advance-rng! (:offset result))
      result)
    nil))

(defn combat-success-probability
  "Estimate the probability of winning combat with current setup.

   Runs multiple simulations and returns win rate.
   Higher is better; 1.0 means certain victory.

   Parameters:
   - game-state: Current game state
   - villain-id: :troll or :thief
   - n: Number of simulations (default 20)"
  [game-state villain-id & {:keys [n] :or {n 20}}]
  (let [saved-state (random/save-state)
        stats (run-combat-simulation game-state villain-id n :max-turns 20)]
    (random/restore-state! saved-state)
    (if (pos? (:total-fights stats))
      (double (/ (:wins stats) (:total-fights stats)))
      0.0)))

(defn find-all-winning-offsets
  "Find all RNG offsets (up to max-offset) that produce wins.

   Useful for understanding RNG distribution and picking optimal offsets.
   Returns vector of {:offset n :turns t :weapon-drops w}."
  [game-state villain-id & {:keys [max-offset max-turns]
                             :or {max-offset 50 max-turns 20}}]
  (let [saved-state (random/save-state)
        winner (:winner game-state)
        weapon-id (combat-engine/find-weapon game-state winner)
        setup-state (-> game-state
                        (assoc-in [:objects villain-id :strength] 2)
                        (assoc-in [:objects winner :strength] 0))]
    (try
      (vec
       (for [offset (range max-offset)
             :let [_ (random/restore-state! saved-state)
                   _ (random/advance-rng! offset)
                   result (simulate-fight setup-state villain-id weapon-id max-turns)]
             :when (= :win (:outcome result))]
         {:offset offset
          :turns (:turns result)
          :weapon-drops (:weapon-drops result)}))
      (finally
        (random/restore-state! saved-state)))))

(defn best-winning-offset
  "Find the winning offset with fewest turns.

   Returns the offset that wins in minimum turns."
  [game-state villain-id & {:keys [max-offset] :or {max-offset 50}}]
  (let [wins (find-all-winning-offsets game-state villain-id :max-offset max-offset)]
    (when (seq wins)
      (apply min-key :turns wins))))

;;; ---------------------------------------------------------------------------
;;; INTEGRATION WITH EXECUTOR
;;; ---------------------------------------------------------------------------

(defn ensure-combat-success!
  "Prepare RNG for successful combat and return info.

   This is the main entry point for the executor to guarantee combat wins.
   Call immediately before executing combat goals.

   Returns:
   - {:success? true :offset N :turns T} - Ready to win
   - {:success? false :reason :xxx} - Cannot guarantee win

   IMPORTANT: After a successful call, execute combat immediately!"
  [game-state villain-id]
  (let [ready-check (check-combat-ready game-state villain-id)]
    (cond
      ;; Cyclops doesn't need RNG - just say ulysses
      (= villain-id :cyclops)
      {:success? true :method :say-ulysses}

      ;; Not ready for combat
      (not (:ready? ready-check))
      {:success? false
       :reason :not-ready
       :issues (:issues ready-check)}

      ;; Find winning offset
      :else
      (if-let [result (prepare-winning-combat! game-state villain-id)]
        {:success? true
         :offset (:offset result)
         :turns (:turns result)}
        {:success? false
         :reason :no-winning-offset
         :villain villain-id}))))
