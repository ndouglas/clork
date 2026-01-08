#!/usr/bin/env lein exec
;; Script to help build the playthrough JSON
;; Run with: lein exec test/scripts/build-playthrough.clj

(ns build-playthrough
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clork.game-state :as gs]
            [clork.core :as core]
            [clork.parser :as parser]
            [clork.verb-defs :as verb-defs]
            [clork.utils :as utils]
            [clork.daemon :as daemon]
            [clork.random :as random]))

(def meta-verbs
  #{:verbose :brief :super-brief :version :diagnose :score :quit :verify
    :restart :save :restore :script :unscript})

(defn- increment-moves-if-needed
  [game-state]
  (let [action (get-in game-state [:parser :prsa])]
    (if (contains? meta-verbs action)
      game-state
      (update game-state :moves (fnil inc 0)))))

(defn normalize-output [s]
  (when s
    (let [paragraphs (str/split (str/trim s) #"\n\n+")
          normalized (map (fn [para]
                            (-> para
                                str/trim
                                (str/replace #"\n" " ")
                                (str/replace #"  +" " ")))
                          paragraphs)]
      (str/join "\n\n" normalized))))

(defn execute-command [game-state input]
  (let [output (java.io.StringWriter.)]
    (binding [*out* output]
      (let [lexv (parser/lexv-from-input input)
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
            gs (parser/parse-command gs)]
        (if (parser/get-parser-error gs)
          [gs (str output)]
          (let [gs (verb-defs/perform gs)
                gs (increment-moves-if-needed gs)
                gs (if (:clock-wait gs)
                     (dissoc gs :clock-wait)
                     (daemon/clocker gs))
                gs (utils/crlf gs)]
            [gs (str output)]))))))

(defn create-initial-state [seed]
  (random/init! seed)
  (let [output (java.io.StringWriter.)]
    (binding [*out* output]
      (core/init-game nil))))

(defn run-commands
  "Run a list of commands and return the final state and outputs."
  [seed commands]
  (random/init! seed)
  (loop [gs (create-initial-state seed)
         remaining commands
         outputs []
         cmd-num 1]
    (if (empty? remaining)
      {:game-state gs
       :outputs outputs
       :commands-run (dec cmd-num)}
      (let [cmd (first remaining)
            [new-gs output-raw] (execute-command gs cmd)
            output (normalize-output output-raw)]
        (println (str "#" cmd-num ": " cmd))
        (println output)
        (println)
        (recur new-gs (rest remaining) (conj outputs {:command cmd :response output}) (inc cmd-num))))))

(defn show-status
  "Show current game status after running commands."
  [result]
  (let [gs (:game-state result)]
    (println "=== Status ===")
    (println "Location:" (:here gs))
    (println "Score:" (:score gs 0))
    (println "Moves:" (:moves gs 0))
    (println "Inventory:" (gs/get-contents gs :adventurer))))

;; Speedrun commands - Phase 1: Get egg and setup
(def phase1-commands
  ["n"           ; To Forest Path
   "n"           ; To Clearing
   "u"           ; Climb tree
   "take egg"    ; First treasure
   "d"           ; Down from tree
   "s"           ; Back to Path
   "e"           ; Behind House
   "open window" ; Access house
   "w"           ; Enter Kitchen
   "w"           ; To Living Room
   "take lamp"   ; Essential light
   "take sword"  ; For combat
   "move rug"    ; Reveal trapdoor
   "open case"   ; Ready for treasures
   "open trap door" ; Ready to descend
   ])

;; Run phase 1
(def result (run-commands 42 phase1-commands))
(show-status result)

;; Generate JSON for these commands
(println "\n=== JSON Output ===")
(println (json/write-str {:description "A complete playthrough of Clork with seed 42"
                          :seed 42
                          :header ""  ; Will be filled
                          :commands (:outputs result)}
                         :escape-slash false))
