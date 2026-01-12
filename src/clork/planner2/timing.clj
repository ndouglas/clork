(ns clork.planner2.timing
  "Timing validation and resource tracking for speedrun planning.

   Critical timing constraints in Zork I:
   1. Exorcism sequence: Bell -> 6 turns -> Candles -> 3 turns -> Book
   2. Candle lifespan: ~40 turns of light
   3. Lantern battery: ~200 turns of light
   4. Dam draining: 8 turns after opening gates

   This module:
   - Tracks consumable resources (lantern, candles)
   - Validates time-critical sequences
   - Warns when timing constraints might be violated
   - Calculates safe execution windows"
  (:require [clork.planner2.observe :as obs]
            [clork.planner2.prep :as prep]
            [clork.game-state :as gs]))

;;; ---------------------------------------------------------------------------
;;; RESOURCE CONSTANTS
;;; ---------------------------------------------------------------------------

(def lantern-max-turns
  "Maximum turns the lantern can provide light."
  200)

(def candles-max-turns
  "Maximum turns the candles can provide light."
  40)

(def dam-drain-turns
  "Turns after opening gates until reservoir drains."
  8)

(def exorcism-bell-window
  "Turns after ringing bell to light candles."
  6)

(def exorcism-candles-window
  "Turns after lighting candles to read book."
  3)

;;; ---------------------------------------------------------------------------
;;; RESOURCE STATE TRACKING
;;; ---------------------------------------------------------------------------

(defrecord ResourceState
  [lantern-remaining   ; Turns of lantern light left
   candles-remaining   ; Turns of candles light left
   candles-lit?        ; Are candles currently providing light?
   lantern-on?         ; Is lantern currently on?
   dam-timer           ; Turns until dam drains (nil if not started)
   exorcism-state])    ; :none, :bell-rang, :candles-lit, :complete

(defn make-resource-state
  "Create initial resource state."
  []
  (->ResourceState lantern-max-turns candles-max-turns false false nil :none))

(defn observe-resource-state
  "Observe current resource state from game state."
  [game-state]
  (let [lantern-on (obs/lantern-on? game-state)
        ;; Estimate remaining lantern based on turn count
        ;; This is approximate - actual implementation would track usage
        turn-count (get game-state :turn-count 0)
        lantern-used (if lantern-on turn-count 0)
        ;; Check candles
        candles-lit (gs/set-thing-flag? game-state :candles :on)
        ;; Check dam state
        gates-open (get game-state :gates-open)
        low-tide (get game-state :low-tide)]
    (->ResourceState
     (max 0 (- lantern-max-turns lantern-used))
     candles-max-turns  ; Hard to track without more state
     candles-lit
     lantern-on
     (cond
       low-tide nil
       gates-open dam-drain-turns
       :else nil)
     (cond
       (get game-state :lld-flag) :complete
       (get game-state :candles-lit) :candles-lit
       (get game-state :bell-rang) :bell-rang
       :else :none))))

;;; ---------------------------------------------------------------------------
;;; TIMING VALIDATION
;;; ---------------------------------------------------------------------------

(defn validate-exorcism-sequence
  "Validate that an exorcism sequence can complete within time limits.

   Parameters:
   - turns-to-bell: Estimated turns to reach entrance and ring bell
   - turns-to-candles: Turns between bell and lighting candles
   - turns-to-book: Turns between candles and reading book

   Returns {:valid? bool, :errors [...], :warnings [...]}"
  [turns-to-bell turns-to-candles turns-to-book]
  (let [errors []
        warnings []
        ;; Validate candles timing
        errors (if (> turns-to-candles exorcism-bell-window)
                 (conj errors
                       {:type :exorcism-timeout
                        :phase :candles
                        :planned turns-to-candles
                        :max exorcism-bell-window
                        :msg (str "Candles must be lit within " exorcism-bell-window
                                  " turns of bell, planned: " turns-to-candles)})
                 errors)
        ;; Validate book timing
        errors (if (> turns-to-book exorcism-candles-window)
                 (conj errors
                       {:type :exorcism-timeout
                        :phase :book
                        :planned turns-to-book
                        :max exorcism-candles-window
                        :msg (str "Book must be read within " exorcism-candles-window
                                  " turns of candles, planned: " turns-to-book)})
                 errors)
        ;; Warnings for tight margins
        warnings (cond-> warnings
                   (and (<= turns-to-candles exorcism-bell-window)
                        (> turns-to-candles (- exorcism-bell-window 2)))
                   (conj {:type :tight-margin
                          :phase :candles
                          :msg "Only 1-2 turns margin for lighting candles"})

                   (and (<= turns-to-book exorcism-candles-window)
                        (> turns-to-book (- exorcism-candles-window 1)))
                   (conj {:type :tight-margin
                          :phase :book
                          :msg "Only 1 turn margin for reading book"}))]
    {:valid? (empty? errors)
     :errors (vec errors)
     :warnings (vec warnings)}))

(defn validate-lantern-usage
  "Validate that a plan's underground segments fit within lantern battery.

   Parameters:
   - underground-turns: Total turns planned for underground exploration
   - current-remaining: Current lantern battery remaining

   Returns {:valid? bool, :errors [...], :warnings [...]}"
  [underground-turns current-remaining]
  (let [safety-margin 20  ; Leave some buffer
        effective-remaining (- current-remaining safety-margin)
        errors []
        warnings []]
    (cond
      (> underground-turns current-remaining)
      {:valid? false
       :errors [{:type :lantern-exhaustion
                 :planned underground-turns
                 :remaining current-remaining
                 :msg (str "Plan requires " underground-turns " underground turns, "
                           "but only " current-remaining " lantern turns remain")}]
       :warnings []}

      (> underground-turns effective-remaining)
      {:valid? true
       :errors []
       :warnings [{:type :low-margin
                   :planned underground-turns
                   :remaining current-remaining
                   :msg (str "Plan leaves only "
                             (- current-remaining underground-turns)
                             " lantern turns buffer")}]}

      :else
      {:valid? true :errors [] :warnings []})))

(defn validate-candle-usage
  "Validate that candle-requiring segments fit within candle lifespan.

   The main candle usage is the exorcism sequence, which requires:
   - Candles lit for the entire bell -> book sequence
   - Approximately 10-15 turns minimum"
  [candle-turns current-remaining]
  (cond
    (> candle-turns current-remaining)
    {:valid? false
     :errors [{:type :candles-exhaustion
               :planned candle-turns
               :remaining current-remaining
               :msg (str "Plan requires " candle-turns " turns with candles, "
                         "but only " current-remaining " remain")}]
     :warnings []}

    (> candle-turns (- current-remaining 10))
    {:valid? true
     :errors []
     :warnings [{:type :low-candle-margin
                 :msg "Candle margin is tight - minimize non-essential candle use"}]}

    :else
    {:valid? true :errors [] :warnings []}))

(defn validate-dam-timing
  "Validate that activities requiring low tide are planned correctly.

   The dam drains 8 turns after opening gates. Plan should:
   - Open gates
   - Do other work for 8 turns
   - Then access reservoir/low-tide areas"
  [turns-after-gates low-tide-activities]
  (let [errors []
        warnings []]
    (cond
      (< turns-after-gates dam-drain-turns)
      {:valid? false
       :errors [{:type :dam-not-drained
                 :planned turns-after-gates
                 :required dam-drain-turns
                 :msg (str "Low tide activities planned " turns-after-gates
                           " turns after gates, but dam needs " dam-drain-turns
                           " turns to drain")}]
       :warnings []}

      (= turns-after-gates dam-drain-turns)
      {:valid? true
       :errors []
       :warnings [{:type :no-dam-margin
                   :msg "No margin for dam timing - any delay will fail"}]}

      :else
      {:valid? true :errors [] :warnings []})))

;;; ---------------------------------------------------------------------------
;;; PLAN TIMING ANALYSIS
;;; ---------------------------------------------------------------------------

(defrecord TimingAnalysis
  [total-turns         ; Estimated total turns
   underground-turns   ; Turns spent underground (using lantern)
   exorcism-valid?     ; Can exorcism complete in time?
   lantern-valid?      ; Enough lantern battery?
   candle-valid?       ; Enough candle life?
   dam-valid?          ; Dam timing correct?
   errors              ; All timing errors
   warnings])          ; All timing warnings

(defn analyze-plan-timing
  "Analyze timing constraints for a full plan.

   Parameters:
   - plan-segments: Vector of {:type :underground/:surface/:exorcism/:dam,
                               :turns N, ...}
   - resource-state: Current ResourceState

   Returns TimingAnalysis."
  [plan-segments resource-state]
  (let [;; Sum up turns by type
        total-turns (reduce + (map :turns plan-segments))
        underground-turns (->> plan-segments
                               (filter #(= :underground (:type %)))
                               (map :turns)
                               (reduce + 0))

        ;; Validate each constraint
        lantern-check (validate-lantern-usage underground-turns
                                               (:lantern-remaining resource-state))
        candle-check (validate-candle-usage
                      (->> plan-segments
                           (filter #(= :exorcism (:type %)))
                           (map :turns)
                           (reduce + 0))
                      (:candles-remaining resource-state))

        ;; Find exorcism segment
        exorcism-seg (first (filter #(= :exorcism (:type %)) plan-segments))
        exorcism-check (if exorcism-seg
                         (validate-exorcism-sequence
                          0  ; Already at location
                          (get exorcism-seg :turns-to-candles 1)
                          (get exorcism-seg :turns-to-book 1))
                         {:valid? true :errors [] :warnings []})

        ;; Find dam segment
        dam-seg (first (filter #(= :dam (:type %)) plan-segments))
        dam-check (if dam-seg
                    (validate-dam-timing
                     (get dam-seg :turns-after-gates 8)
                     [])
                    {:valid? true :errors [] :warnings []})

        ;; Aggregate errors and warnings
        all-errors (concat (:errors lantern-check)
                           (:errors candle-check)
                           (:errors exorcism-check)
                           (:errors dam-check))
        all-warnings (concat (:warnings lantern-check)
                             (:warnings candle-check)
                             (:warnings exorcism-check)
                             (:warnings dam-check))]

    (->TimingAnalysis
     total-turns
     underground-turns
     (:valid? exorcism-check)
     (:valid? lantern-check)
     (:valid? candle-check)
     (:valid? dam-check)
     (vec all-errors)
     (vec all-warnings))))

;;; ---------------------------------------------------------------------------
;;; SAFE EXECUTION WINDOWS
;;; ---------------------------------------------------------------------------

(defn exorcism-execution-window
  "Calculate the safe execution window for exorcism.

   Returns {:start turn, :end turn, :optimal turn} relative to current turn."
  [resource-state]
  (let [candle-remaining (:candles-remaining resource-state)
        ;; Need at least 15 turns of candles for safe exorcism
        min-candles-needed 15]
    (if (< candle-remaining min-candles-needed)
      {:error "Insufficient candles for exorcism"
       :remaining candle-remaining
       :needed min-candles-needed}
      {:start 0
       :end (- candle-remaining min-candles-needed)
       :optimal 0
       :note "Execute exorcism as soon as prerequisites are met"})))

(defn dam-execution-window
  "Calculate when to start dam puzzle for optimal parallel work.

   The dam takes 8 turns to drain. During this time, do other work.

   Parameters:
   - parallel-work-turns: Turns of other work available to do

   Returns {:open-gates-at turn, :access-reservoir-at turn}"
  [parallel-work-turns]
  (let [;; If we have more than 8 turns of parallel work, open gates immediately
        ;; If less, we'll have to wait
        wait-turns (max 0 (- dam-drain-turns parallel-work-turns))]
    {:open-gates-at 0
     :do-parallel-work parallel-work-turns
     :wait-if-needed wait-turns
     :access-reservoir-at (+ parallel-work-turns wait-turns)
     :note (if (>= parallel-work-turns dam-drain-turns)
             "Enough parallel work to fill dam drain time"
             (str "Need to wait " wait-turns " extra turns after parallel work"))}))

(defn lantern-budget
  "Calculate lantern budget for underground exploration.

   Returns breakdown of how to allocate lantern turns."
  [resource-state required-underground-turns]
  (let [remaining (:lantern-remaining resource-state)
        safety-margin 20
        available (- remaining safety-margin)]
    {:remaining remaining
     :safety-margin safety-margin
     :available-for-plan available
     :required required-underground-turns
     :surplus (- available required-underground-turns)
     :valid? (>= available required-underground-turns)
     :recommendation (cond
                       (< remaining required-underground-turns)
                       "CRITICAL: Not enough lantern - optimize route or skip treasures"

                       (< available required-underground-turns)
                       "WARNING: Tight margin - minimize exploration, stay on route"

                       (> (- available required-underground-turns) 50)
                       "Comfortable margin - some exploration allowed"

                       :else
                       "Adequate margin - stick to planned route")}))

;;; ---------------------------------------------------------------------------
;;; FORMAT OUTPUT
;;; ---------------------------------------------------------------------------

(defn format-timing-analysis
  "Format timing analysis for display."
  [analysis]
  (str "=== Timing Analysis ===\n"
       "Total turns: " (:total-turns analysis) "\n"
       "Underground turns: " (:underground-turns analysis) "\n"
       "\nConstraint Status:\n"
       "  Exorcism: " (if (:exorcism-valid? analysis) "OK" "FAIL") "\n"
       "  Lantern: " (if (:lantern-valid? analysis) "OK" "FAIL") "\n"
       "  Candles: " (if (:candle-valid? analysis) "OK" "FAIL") "\n"
       "  Dam: " (if (:dam-valid? analysis) "OK" "FAIL") "\n"
       (when (seq (:errors analysis))
         (str "\nErrors:\n"
              (clojure.string/join "\n" (map #(str "  - " (:msg %)) (:errors analysis)))))
       (when (seq (:warnings analysis))
         (str "\nWarnings:\n"
              (clojure.string/join "\n" (map #(str "  - " (:msg %)) (:warnings analysis)))))))

(defn format-lantern-budget
  "Format lantern budget for display."
  [budget]
  (str "=== Lantern Budget ===\n"
       "Remaining: " (:remaining budget) " turns\n"
       "Safety margin: " (:safety-margin budget) " turns\n"
       "Available: " (:available-for-plan budget) " turns\n"
       "Required: " (:required budget) " turns\n"
       "Surplus: " (:surplus budget) " turns\n"
       "Status: " (if (:valid? budget) "VALID" "INVALID") "\n"
       "Recommendation: " (:recommendation budget)))

(defn print-timing-analysis
  "Print timing analysis."
  [analysis]
  (println (format-timing-analysis analysis)))

(defn print-lantern-budget
  "Print lantern budget."
  [budget]
  (println (format-lantern-budget budget)))

