(ns clork.planner.validator
  "Validates planner-generated paths by executing them through the game engine.

   This ensures that navigation paths actually work, catching issues like:
   - Doors that need to be opened
   - Conditions not properly modeled in the navigation graph
   - Parser edge cases
   - Combat victory detection (enemy dies before max attacks)"
  (:require [clork.parser :as parser]
            [clork.verb-defs :as verb-defs]
            [clork.daemon :as daemon]
            [clork.utils :as utils]
            [clojure.string :as str]))

;; =============================================================================
;; Vocabulary Registration
;; =============================================================================

(defonce ^:private vocab-registered? (atom false))

(defn ensure-vocabulary-registered!
  "Ensure object vocabulary is registered for the parser.
   This is idempotent - calling multiple times is safe.

   Must be called before executing commands, otherwise the parser
   won't recognize object words like 'window', 'sword', etc."
  [game-state]
  (when-not @vocab-registered?
    (verb-defs/register-object-vocabulary! (:objects game-state))
    (reset! vocab-registered? true)))

;; =============================================================================
;; Command Execution
;; =============================================================================

(defn execute-command
  "Execute a single command and return [new-state output-string].
   Simulates one iteration of the main loop without user interaction.

   Ensures object vocabulary is registered before parsing."
  [game-state input]
  ;; Ensure vocabulary is registered (idempotent)
  (ensure-vocabulary-registered! game-state)
  (let [output (java.io.StringWriter.)]
    (binding [*out* output]
      (let [;; Initialize parser state
            lexv (parser/lexv-from-input input)
            gs (-> game-state
                   (parser/parser-init)
                   (parser/parser-set-winner-to-player)
                   (assoc :input input)
                   (assoc-in [:parser :lexv] lexv)
                   (assoc-in [:parser :len] (count (:tokens lexv)))
                   (assoc-in [:parser :again-lexv] lexv)
                   (assoc-in [:parser :dir] nil)
                   (assoc-in [:parser :ncn] 0)
                   (assoc-in [:parser :getflags] 0))
            ;; Parse the command
            gs (parser/parse-command gs)]
        (if (parser/get-parser-error gs)
          ;; Parsing failed - return error
          [gs (str output)]
          ;; Parsing succeeded - perform the action, then run daemons
          (let [gs (verb-defs/perform gs)
                ;; Check if verb already handled clocker (e.g., wait)
                gs (if (:clock-wait gs)
                     (dissoc gs :clock-wait)
                     (daemon/clocker gs))
                gs (utils/crlf gs)]
            [gs (str output)]))))))

(defn execute-commands
  "Execute a sequence of commands, returning trace of each step.

   Returns:
   {:success? boolean
    :final-state game-state
    :final-room room-id
    :steps [{:command str :from room :to room :output str :error? bool}...]
    :error (if failed) {:step n :command str :message str}}"
  [game-state commands]
  (loop [gs game-state
         remaining commands
         steps []
         step-num 0]
    (if (empty? remaining)
      ;; All commands executed successfully
      {:success? true
       :final-state gs
       :final-room (:here gs)
       :steps steps
       :commands-executed (count commands)}
      ;; Execute next command
      (let [cmd (first remaining)
            from-room (:here gs)
            [new-gs output] (execute-command gs cmd)
            to-room (:here new-gs)
            parser-error (parser/get-parser-error new-gs)
            step-info {:step step-num
                       :command cmd
                       :from from-room
                       :to to-room
                       :output output
                       :error? (some? parser-error)}]
        (if parser-error
          ;; Command failed
          {:success? false
           :final-state new-gs
           :final-room to-room
           :steps (conj steps step-info)
           :commands-executed step-num
           :error {:step step-num
                   :command cmd
                   :from from-room
                   :message (str "Parser error: " (:type parser-error))
                   :details parser-error}}
          ;; Command succeeded, continue
          (recur new-gs
                 (rest remaining)
                 (conj steps step-info)
                 (inc step-num)))))))

;; =============================================================================
;; Combat-Aware Execution
;; =============================================================================

(defn- attack-command?
  "Check if a command is an attack command."
  [cmd]
  (and (string? cmd)
       (or (str/starts-with? cmd "attack ")
           (str/starts-with? cmd "kill ")
           (str/starts-with? cmd "hit "))))

(defn- extract-attack-target
  "Extract the target from an attack command.
   'attack troll with sword' -> 'troll'"
  [cmd]
  (when (attack-command? cmd)
    (let [;; Remove 'attack ', 'kill ', or 'hit '
          without-verb (-> cmd
                           (str/replace #"^attack\s+" "")
                           (str/replace #"^kill\s+" "")
                           (str/replace #"^hit\s+" ""))
          ;; Remove 'with X' suffix
          target (str/replace without-verb #"\s+with\s+.*$" "")]
      (str/trim target))))

(defn- combat-victory-error?
  "Check if a parser error indicates combat victory (target no longer exists).
   Returns true for errors like 'can't see any such thing'."
  [parser-error output]
  (or (= :not-here (:type parser-error))
      (and (string? output)
           (or (str/includes? (str/lower-case output) "can't see any such thing")
               (str/includes? (str/lower-case output) "cannot see any such thing")))))

(defn execute-commands-combat-aware
  "Execute commands with smart combat handling.

   Unlike execute-commands, this function:
   - Detects when combat succeeds early (enemy dies before max attacks)
   - Skips remaining attack commands for a defeated enemy
   - Returns success if all non-redundant commands succeed

   Returns same structure as execute-commands, plus:
   - :combat-victories [{:enemy str :attacks-used n :attacks-skipped n}...]"
  [game-state commands]
  (loop [gs game-state
         remaining commands
         steps []
         step-num 0
         current-combat-target nil  ; Track current combat target
         combat-victories []]
    (if (empty? remaining)
      ;; All commands executed successfully
      {:success? true
       :final-state gs
       :final-room (:here gs)
       :steps steps
       :commands-executed step-num
       :combat-victories combat-victories}
      ;; Execute next command
      (let [cmd (first remaining)
            cmd-target (extract-attack-target cmd)

            ;; Check if we should skip this command (attacking already-dead enemy)
            skip-command? (and cmd-target
                              (some #(= (:enemy %) cmd-target) combat-victories))]
        (if skip-command?
          ;; Skip this attack - enemy already defeated
          (let [;; Update the victory record with skipped count
                updated-victories (mapv (fn [v]
                                          (if (= (:enemy v) cmd-target)
                                            (update v :attacks-skipped inc)
                                            v))
                                        combat-victories)]
            (recur gs
                   (rest remaining)
                   steps
                   step-num  ; Don't increment - command was skipped
                   current-combat-target
                   updated-victories))

          ;; Execute the command
          (let [from-room (:here gs)
                [new-gs output] (execute-command gs cmd)
                to-room (:here new-gs)
                parser-error (parser/get-parser-error new-gs)
                step-info {:step step-num
                           :command cmd
                           :from from-room
                           :to to-room
                           :output output
                           :error? (some? parser-error)}]
            (cond
              ;; No error - command succeeded
              (nil? parser-error)
              (recur new-gs
                     (rest remaining)
                     (conj steps step-info)
                     (inc step-num)
                     cmd-target  ; Update current combat target
                     combat-victories)

              ;; Attack command failed with "can't see" - combat victory!
              (and cmd-target (combat-victory-error? parser-error output))
              (let [victory {:enemy cmd-target
                             :attacks-used step-num  ; How many attacks before this one
                             :attacks-skipped 0}]
                (recur gs  ; Keep previous state (before failed attack)
                       (rest remaining)
                       steps  ; Don't add failed step
                       step-num  ; Don't increment
                       nil  ; Clear combat target
                       (conj combat-victories victory)))

              ;; Other error - real failure
              :else
              {:success? false
               :final-state new-gs
               :final-room to-room
               :steps (conj steps step-info)
               :commands-executed step-num
               :error {:step step-num
                       :command cmd
                       :from from-room
                       :message (str "Parser error: " (:type parser-error))
                       :details parser-error}
               :combat-victories combat-victories})))))))

;; =============================================================================
;; Navigation Validation
;; =============================================================================

(defn validate-navigation
  "Validate that a navigation path actually reaches its destination.

   Parameters:
   - game-state: Initial game state (should be at start-room)
   - start-room: Expected starting room
   - dest-room: Expected destination room
   - commands: Navigation commands to execute

   Returns:
   {:valid? boolean
    :start-room room
    :dest-room room
    :actual-end-room room
    :commands-executed n
    :error (if invalid) {:type :wrong-start|:failed-command|:wrong-destination
                          ...details}}"
  [game-state start-room dest-room commands]
  ;; Verify starting position
  (if (not= (:here game-state) start-room)
    {:valid? false
     :start-room start-room
     :dest-room dest-room
     :actual-end-room (:here game-state)
     :commands-executed 0
     :error {:type :wrong-start
             :expected start-room
             :actual (:here game-state)
             :message (str "Expected to start at " start-room
                          " but game state is at " (:here game-state))}}
    ;; Execute navigation commands
    (let [result (execute-commands game-state commands)
          final-room (:final-room result)]
      (cond
        ;; Command execution failed
        (not (:success? result))
        {:valid? false
         :start-room start-room
         :dest-room dest-room
         :actual-end-room final-room
         :commands-executed (:commands-executed result)
         :error (assoc (:error result) :type :failed-command)
         :steps (:steps result)}

        ;; Reached wrong destination
        (not= final-room dest-room)
        {:valid? false
         :start-room start-room
         :dest-room dest-room
         :actual-end-room final-room
         :commands-executed (:commands-executed result)
         :error {:type :wrong-destination
                 :expected dest-room
                 :actual final-room
                 :message (str "Expected to reach " dest-room
                              " but ended up at " final-room)}
         :steps (:steps result)}

        ;; Success!
        :else
        {:valid? true
         :start-room start-room
         :dest-room dest-room
         :actual-end-room final-room
         :commands-executed (:commands-executed result)
         :final-state (:final-state result)
         :steps (:steps result)}))))

;; =============================================================================
;; Plan Validation
;; =============================================================================

(defn validate-action-navigation
  "Validate navigation to an action's location.

   Parameters:
   - game-state: Current game state
   - dest-room: Where the action needs to be performed
   - nav-commands: Commands to get there

   Returns validation result with :valid? boolean."
  [game-state dest-room nav-commands]
  (if (empty? nav-commands)
    ;; No navigation needed - should already be at destination
    (if (= (:here game-state) dest-room)
      {:valid? true
       :start-room (:here game-state)
       :dest-room dest-room
       :actual-end-room dest-room
       :commands-executed 0
       :final-state game-state}
      {:valid? false
       :start-room (:here game-state)
       :dest-room dest-room
       :actual-end-room (:here game-state)
       :commands-executed 0
       :error {:type :wrong-location
               :expected dest-room
               :actual (:here game-state)
               :message (str "Action requires being at " dest-room
                            " but player is at " (:here game-state))}})
    ;; Validate the navigation
    (validate-navigation game-state (:here game-state) dest-room nav-commands)))

(defn validate-plan-navigations
  "Validate all navigation segments in a plan.

   Takes the output of plan-to-command-sequence and validates each
   navigation segment by actually running it through the game.

   Parameters:
   - game-state: Initial game state
   - by-action: The :by-action output from plan-to-command-sequence

   Returns:
   {:valid? boolean
    :validations [{:action action-id :valid? bool :details...}...]
    :first-failure (if any) {:action action-id :error...}}"
  [game-state by-action]
  (loop [gs game-state
         remaining by-action
         validations []]
    (if (empty? remaining)
      {:valid? true
       :validations validations
       :all-passed (count validations)}
      (let [{:keys [action to nav-commands action-commands]} (first remaining)
            ;; Only validate if there's a destination and navigation
            needs-validation? (and to (seq nav-commands))
            validation (when needs-validation?
                        (validate-action-navigation gs to nav-commands))]
        (if (and validation (not (:valid? validation)))
          ;; Navigation failed
          {:valid? false
           :validations (conj validations {:action action
                                           :valid? false
                                           :details validation})
           :first-failure {:action action
                          :error (:error validation)
                          :from (:here gs)
                          :to to
                          :nav-commands nav-commands}}
          ;; Navigation succeeded or not needed
          ;; Simulate being at destination and execute action commands
          (let [new-gs (if validation
                        (:final-state validation)
                        gs)
                ;; Execute action commands to update state
                action-result (when (seq action-commands)
                               (execute-commands new-gs action-commands))
                final-gs (if action-result
                          (:final-state action-result)
                          new-gs)]
            (recur final-gs
                   (rest remaining)
                   (conj validations {:action action
                                     :valid? true
                                     :nav-validated? needs-validation?
                                     :dest to}))))))))

;; =============================================================================
;; Debug Utilities
;; =============================================================================

(defn print-validation-result
  "Pretty print a validation result."
  [{:keys [valid? start-room dest-room actual-end-room
           commands-executed error steps] :as result}]
  (println (str "\n=== Navigation Validation: " (if valid? "PASSED" "FAILED") " ==="))
  (println (str "  Route: " start-room " -> " dest-room))
  (println (str "  Commands executed: " commands-executed))
  (when (not= dest-room actual-end-room)
    (println (str "  Actual end: " actual-end-room)))
  (when error
    (println (str "  Error type: " (:type error)))
    (println (str "  Message: " (:message error))))
  (when (and (not valid?) steps)
    (println "  Steps:")
    (doseq [{:keys [step command from to error?]} steps]
      (println (str "    " step ". " command
                   " (" from " -> " to ")"
                   (when error? " [ERROR]"))))))

(defn print-plan-validation
  "Pretty print plan validation results."
  [{:keys [valid? validations first-failure all-passed]}]
  (println (str "\n=== Plan Navigation Validation: "
               (if valid? "ALL PASSED" "FAILED") " ==="))
  (if valid?
    (println (str "  " all-passed " navigation(s) validated successfully"))
    (do
      (println (str "  Failed at action: " (:action first-failure)))
      (println (str "  Route: " (:from first-failure) " -> " (:to first-failure)))
      (println (str "  Commands: " (:nav-commands first-failure)))
      (println (str "  Error: " (get-in first-failure [:error :message]))))))
