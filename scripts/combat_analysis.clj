#!/usr/bin/env bb
;; Combat Analysis Script
;; Simulates combat with troll and thief at different score levels (0-350)
;; and graphs the win/death rates.

(ns combat-analysis
  (:require [clojure.string :as str]))

;; Load the project
(require '[clork.core :as core])
(require '[clork.planner2.combat :as combat-sim])
(require '[clork.game-state :as gs])
(require '[clork.random :as random])

(def simulations-per-score 500)  ; Number of fights to simulate at each score level
(def score-step 10)              ; Score increment (0, 10, 20, ... 350)

;; Initialize RNG with a fixed seed for reproducibility
(random/init! 42)

(defn setup-game-state-at-score
  "Create a game state with a specific score."
  [score]
  (-> (core/init-game)
      (assoc :score score)))

(defn run-combat-simulation-fixed
  "Fixed version of run-combat-simulation that uses correct villain strengths.
   Troll has strength 2, Thief has strength 5."
  [game-state villain-id n & {:keys [max-turns] :or {max-turns 50}}]
  (let [;; Use correct villain strengths from the game
        villain-strength (case villain-id
                           :troll 2
                           :thief 5
                           2)
        villain-room (case villain-id
                       :troll :troll-room
                       :thief :round-room
                       nil)
        weapon-id :sword
        winner (:winner game-state)

        ;; Setup initial state - ensure villain has their weapon
        setup-state (-> game-state
                        (assoc :here villain-room)
                        (assoc-in [:objects villain-id :strength] villain-strength)
                        (assoc-in [:objects villain-id :in] villain-room)
                        (assoc-in [:objects weapon-id :in] winner)
                        ;; Make sure thief has stiletto
                        (cond-> (= villain-id :thief)
                          (assoc-in [:objects :stiletto :in] :thief)))]

    ;; Run N fights
    (reduce
     (fn [stats _]
       (let [;; Reset for each fight
             fresh-state (-> setup-state
                             (assoc-in [:objects villain-id :strength] villain-strength)
                             (assoc-in [:objects villain-id :in] villain-room)
                             (assoc-in [:objects weapon-id :in] winner)
                             (assoc-in [:objects winner :strength] 0)
                             (gs/unset-thing-flag villain-id :staggered)
                             (gs/unset-thing-flag winner :staggered))
             result (combat-sim/simulate-fight fresh-state villain-id weapon-id max-turns)]
         (combat-sim/update-stats stats result)))
     (combat-sim/make-combat-stats)
     (range n))))

(defn run-simulations-at-score
  "Run combat simulations for both troll and thief at a given score.
   Returns {:score N :troll {:wins % :deaths %} :thief {:wins % :deaths %}}"
  [score]
  (let [gs (setup-game-state-at-score score)
        ;; Save RNG state and restore after each simulation to ensure independence
        saved-rng (random/save-state)

        ;; Run troll simulations (troll strength = 2)
        _ (random/restore-state! saved-rng)
        troll-stats (run-combat-simulation-fixed gs :troll simulations-per-score)

        ;; Run thief simulations (thief strength = 5)
        _ (random/restore-state! saved-rng)
        _ (random/advance-rng! 10000)  ; Use different RNG sequence
        thief-stats (run-combat-simulation-fixed gs :thief simulations-per-score)

        ;; Calculate percentages
        troll-win-pct (if (pos? (:total-fights troll-stats))
                        (* 100.0 (/ (:wins troll-stats) (:total-fights troll-stats)))
                        0.0)
        troll-death-pct (if (pos? (:total-fights troll-stats))
                          (* 100.0 (/ (:deaths troll-stats) (:total-fights troll-stats)))
                          0.0)
        thief-win-pct (if (pos? (:total-fights thief-stats))
                        (* 100.0 (/ (:wins thief-stats) (:total-fights thief-stats)))
                        0.0)
        thief-death-pct (if (pos? (:total-fights thief-stats))
                          (* 100.0 (/ (:deaths thief-stats) (:total-fights thief-stats)))
                          0.0)]

    {:score score
     :strength (:player-strength troll-stats)
     :troll {:win-pct troll-win-pct
             :death-pct troll-death-pct
             :avg-turns (:avg-turns troll-stats)}
     :thief {:win-pct thief-win-pct
             :death-pct thief-death-pct
             :avg-turns (:avg-turns thief-stats)}}))

(defn render-bar
  "Render an ASCII bar chart bar."
  [value max-value width char]
  (let [bar-len (int (* (/ value max-value) width))]
    (apply str (repeat bar-len char))))

(defn render-graph
  "Render an ASCII graph of the combat data."
  [data]
  (let [graph-width 50]
    (println "\n" (str/join "" (repeat 80 "=")) "\n")
    (println "           COMBAT WIN RATES VS TROLL AND THIEF BY SCORE")
    (println "           " (str simulations-per-score " simulations per data point"))
    (println "\n" (str/join "" (repeat 80 "=")) "\n")

    (println "\n  Score  Str |  TROLL Win %                                    | THIEF Win %")
    (println "  ---------- | ---------------------------------------------- | ------------------------------------------------")

    (doseq [{:keys [score strength troll thief]} data]
      (let [troll-bar (render-bar (:win-pct troll) 100 45 "#")
            thief-bar (render-bar (:win-pct thief) 100 45 "*")]
        (printf "  %3d    %d   | %-45s %5.1f%% | %-45s %5.1f%%\n"
                score strength
                troll-bar (:win-pct troll)
                thief-bar (:win-pct thief))))

    (println "\n  Legend: # = Troll win rate, * = Thief win rate\n")

    ;; Death rate table
    (println "\n" (str/join "" (repeat 80 "-")) "\n")
    (println "           DEATH RATES (probability of player dying)")
    (println "\n  Score  Str |  TROLL Death %                                  | THIEF Death %")
    (println "  ---------- | ---------------------------------------------- | ------------------------------------------------")

    (doseq [{:keys [score strength troll thief]} data]
      (let [troll-bar (render-bar (:death-pct troll) 100 45 "X")
            thief-bar (render-bar (:death-pct thief) 100 45 "x")]
        (printf "  %3d    %d   | %-45s %5.1f%% | %-45s %5.1f%%\n"
                score strength
                troll-bar (:death-pct troll)
                thief-bar (:death-pct thief))))

    (println "\n  Legend: X = Troll death rate, x = Thief death rate\n")

    ;; Summary statistics
    (println "\n" (str/join "" (repeat 80 "=")) "\n")
    (println "           SUMMARY BY PLAYER STRENGTH")
    (println "\n" (str/join "" (repeat 80 "=")) "\n")

    ;; Group by strength
    (let [by-strength (group-by :strength data)]
      (doseq [str-level (sort (keys by-strength))]
        (let [entries (get by-strength str-level)
              score-range (str (apply min (map :score entries)) "-" (apply max (map :score entries)))
              avg-troll-win (/ (reduce + (map #(get-in % [:troll :win-pct]) entries)) (count entries))
              avg-troll-death (/ (reduce + (map #(get-in % [:troll :death-pct]) entries)) (count entries))
              avg-thief-win (/ (reduce + (map #(get-in % [:thief :win-pct]) entries)) (count entries))
              avg-thief-death (/ (reduce + (map #(get-in % [:thief :death-pct]) entries)) (count entries))]
          (printf "  Strength %d (score %s):\n" str-level score-range)
          (printf "    vs Troll: %.1f%% win, %.1f%% death\n" avg-troll-win avg-troll-death)
          (printf "    vs Thief: %.1f%% win, %.1f%% death\n\n" avg-thief-win avg-thief-death))))))

(defn -main
  []
  (println "Running combat simulations...")
  (println "This will take a moment...\n")

  (let [scores (range 0 351 score-step)
        total (count scores)]
    (println (str "Simulating " total " score levels, " simulations-per-score " fights each..."))

    (let [data (vec (for [[idx score] (map-indexed vector scores)]
                      (do
                        (printf "\r  Progress: %d/%d (score %d, strength %d)..."
                                (inc idx) total score
                                (+ 2 (int (/ score 70))))
                        (flush)
                        (run-simulations-at-score score))))]

      (println "\n\nSimulations complete!")
      (render-graph data))))

(-main)
