(ns clork.verb-defs
  "Verb definitions - single source of truth for vocabulary, syntax, and handlers."
  (:require [clork.utils :as utils]
            [clork.game-state :as game-state]
            [clork.verbs :as verbs]
            [clork.verbs-look :as verbs-look]))

;;;; ============================================================================
;;;; VERB DEFINITIONS - Single Source of Truth
;;;; ============================================================================
;;;;
;;;; This file provides a unified way to define verbs. Instead of updating
;;;; three separate places (vocabulary, syntax, handlers), define each verb
;;;; once here and the system derives everything else.
;;;;
;;;; To add a new verb:
;;;; 1. Write the handler function in verbs.clj (e.g., v-my-verb)
;;;; 2. Add an entry to verb-definitions below
;;;; 3. That's it!
;;;;
;;;; ============================================================================

;;; ---------------------------------------------------------------------------
;;; VERB DEFINITION STRUCTURE
;;; ---------------------------------------------------------------------------
;;;
;;; Each verb entry is a map with:
;;;   :words    - Vector of strings that invoke this verb ["take" "get" "grab"]
;;;   :syntax   - Syntax specification (see below)
;;;   :handler  - The function to call (e.g., v-verbose)
;;;
;;; Syntax specification can be:
;;;   {:num-objects 0}                           - No arguments
;;;   {:num-objects 1 :loc1 #{:held :in-room}}   - One object, search locations
;;;   {:num-objects 2 :prep2 :in :loc1 ... :loc2 ...}  - Two objects with prep
;;;
;;; Location flags (for :loc1, :loc2):
;;;   :held      - Search player's inventory
;;;   :carried   - Search containers player is carrying
;;;   :in-room   - Search room floor
;;;   :on-ground - Search containers in room
;;;   :take      - Auto-take if not held
;;;   :many      - Allow "all", "everything"
;;;   :have      - Must already be holding

(def verb-definitions
  "The single source of truth for all verb definitions.

   Each key is the action keyword (used in :prsa).
   Each value defines words, syntax, and handler."

  {;; === Meta/System Verbs ===
   :verbose    {:words   ["verbose"]
                :syntax  {:num-objects 0}
                :handler verbs/v-verbose}

   :brief      {:words   ["brief"]
                :syntax  {:num-objects 0}
                :handler verbs/v-brief}

   :super-brief {:words   ["superbrief" "super-brief"]
                 :syntax  {:num-objects 0}
                 :handler verbs/v-super-brief}

   :version    {:words   ["version"]
                :syntax  {:num-objects 0}
                :handler verbs/v-version}

   ;; === Observation Verbs ===
   :look       {:words   ["look" "l"]
                :syntax  {:num-objects 0}
                :handler verbs-look/v-look}

   :inventory  {:words   ["inventory" "i"]
                :syntax  {:num-objects 0}
                :handler verbs/v-inventory}})

;;; ---------------------------------------------------------------------------
;;; BUILDER FUNCTIONS
;;; ---------------------------------------------------------------------------

(defn- loc-set->bits
  "Convert a set of location keywords to the numeric bits."
  [loc-set]
  (when loc-set
    (reduce (fn [acc loc]
              (bit-or acc (get game-state/search-bits loc 0)))
            0
            loc-set)))

(defn build-vocabulary
  "Build the vocabulary map from verb-definitions.

   Returns a map of word-string -> {:parts-of-speech #{:verb} :verb-value action-kw}"
  [definitions]
  (reduce-kv
   (fn [vocab action-kw {:keys [words]}]
     (reduce (fn [v word]
               (assoc v word {:parts-of-speech #{:verb}
                              :verb-value action-kw}))
             vocab
             words))
   {}
   definitions))

(defn build-syntax-entry
  "Convert a syntax spec map to the internal syntax format."
  [action-kw {:keys [num-objects prep1 prep2 gwim1 gwim2 loc1 loc2]
              :or {num-objects 0}}]
  {:num-objects num-objects
   :prep1       prep1
   :prep2       prep2
   :gwim1       gwim1
   :gwim2       gwim2
   :loc1        (loc-set->bits loc1)
   :loc2        (loc-set->bits loc2)
   :action      action-kw})

(defn build-verb-syntaxes
  "Build the verb-syntaxes map from verb-definitions.

   Returns a map of action-kw -> [syntax-entries]"
  [definitions]
  (reduce-kv
   (fn [syntaxes action-kw {:keys [syntax]}]
     (let [;; Support single syntax or vector of syntaxes
           syntax-list (if (vector? syntax) syntax [syntax])]
       (assoc syntaxes action-kw
              (mapv #(build-syntax-entry action-kw %) syntax-list))))
   {}
   definitions))

(defn build-verb-handlers
  "Build the verb-handlers map from verb-definitions.

   Returns a map of action-kw -> handler-fn"
  [definitions]
  (reduce-kv
   (fn [handlers action-kw {:keys [handler]}]
     (assoc handlers action-kw handler))
   {}
   definitions))

;;; ---------------------------------------------------------------------------
;;; GENERATED MAPS
;;; ---------------------------------------------------------------------------
;;; These are derived from verb-definitions and used by the parser and executor.

(def ^:dynamic *verb-vocabulary*
  "Vocabulary entries for verbs. Merged with object/direction vocabulary."
  (build-vocabulary verb-definitions))

(def ^:dynamic *verb-syntaxes*
  "Syntax patterns for each verb action."
  (build-verb-syntaxes verb-definitions))

(def ^:dynamic *verb-handlers*
  "Handler functions for each verb action."
  (build-verb-handlers verb-definitions))

;;; ---------------------------------------------------------------------------
;;; VERB DISPATCH
;;; ---------------------------------------------------------------------------

(defn perform
  "Execute a verb action.

   ZIL: PERFORM routine in gmain.zil

   Looks up the action in *verb-handlers* and calls the handler function.
   Returns the updated game-state."
  [game-state]
  (let [action (get-in game-state [:parser :prsa])]  ; Used here directly for simplicity
    (if-let [handler (get *verb-handlers* action)]
      (handler game-state)
      (do
        (utils/tell game-state (str "I don't know how to do that. [" action "]\n"))
        game-state))))
