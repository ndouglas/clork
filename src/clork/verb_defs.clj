(ns clork.verb-defs
  "Verb definitions - single source of truth for vocabulary, syntax, and handlers."
  (:require [clork.utils :as utils]
            [clork.game-state :as game-state]
            [clork.verbs :as verbs]
            [clork.verbs-look :as verbs-look]
            [clork.debug.trace :as trace]))

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

   :diagnose   {:words   ["diagnose"]
                :syntax  {:num-objects 0}
                :handler verbs/v-diagnose}

   :score      {:words   ["score"]
                :syntax  {:num-objects 0}
                :handler verbs/v-score}

   :quit       {:words   ["quit" "q"]
                :syntax  {:num-objects 0}
                :handler verbs/v-quit}

   ;; === Observation Verbs ===
   :look       {:words   ["look" "l"]
                :syntax  {:num-objects 0}
                :handler verbs-look/v-look}

   :inventory  {:words   ["inventory" "i"]
                :syntax  {:num-objects 0}
                :handler verbs/v-inventory}

   ;; === Manipulation Verbs ===
   :take       {:words   ["take" "get" "hold" "carry" "remove" "grab" "catch"]
                :syntax  {:num-objects 1
                          :loc1 #{:in-room :on-ground :many}}
                :handler verbs/v-take}

   :read       {:words   ["read" "skim"]
                :syntax  {:num-objects 1
                          :loc1 #{:held :carried :in-room :on-ground :take}}
                :handler verbs/v-read}

   :drop       {:words   ["drop" "throw" "discard"]
                :syntax  {:num-objects 1
                          :loc1 #{:held :many :have}}
                :handler verbs/v-drop}

   :open       {:words   ["open"]
                :syntax  {:num-objects 1
                          :loc1 #{:held :in-room :on-ground :carried}}
                :handler verbs/v-open}

   :examine    {:words   ["examine" "x" "describe" "what" "whats"]
                :syntax  {:num-objects 1
                          :loc1 #{:held :carried :in-room :on-ground :many}}
                :handler verbs/v-examine}

   :look-inside {:words   ["search"]
                 :syntax  {:num-objects 1
                           :loc1 #{:held :carried :in-room :on-ground}}
                 :handler verbs/v-look-inside}

   ;; === Movement Verbs ===
   ;; Walk has multiple syntax patterns routing to different handlers.
   ;; ZIL: WALK is a synonym of GO, RUN, PROCEED, STEP
   ;;   <SYNTAX WALK OBJECT = V-WALK>
   ;;   <SYNTAX WALK AROUND OBJECT = V-WALK-AROUND>
   ;;   <SYNTAX WALK IN OBJECT = V-THROUGH>
   ;;   <SYNTAX WALK THROUGH OBJECT = V-THROUGH>
   :walk       {:words   ["walk" "go" "run" "proceed"]
                :syntax  [;; WALK DIRECTION - walk north, go east
                          {:num-objects 1
                           :loc1 #{}}

                          ;; WALK AROUND OBJECT - walk around house
                          {:num-objects 1
                           :prep1 :around
                           :loc1 #{:in-room :on-ground}
                           :action :walk-around}

                          ;; WALK IN OBJECT - walk in window
                          {:num-objects 1
                           :prep1 :in
                           :loc1 #{:in-room :on-ground}
                           :action :through}

                          ;; WALK THROUGH OBJECT - walk through window
                          {:num-objects 1
                           :prep1 :through
                           :loc1 #{:in-room :on-ground}
                           :action :through}]
                :handler verbs/v-walk}

   ;; ZIL: <SYNTAX ENTER = V-ENTER> (bare enter goes to :in direction)
   ;;      <SYNTAX ENTER OBJECT = V-THROUGH>
   ;; Go through a door, window, or other passageway
   :through    {:words   ["enter" "through"]
                :syntax  {:num-objects 1
                          :loc1 #{:in-room :on-ground}}
                :handler verbs/v-through}

   ;; Handler for walk-around - used when WALK AROUND OBJECT is parsed
   ;; The verb words aren't used directly since this routes through :walk
   :walk-around {:words   []  ; No direct words - reached via :walk syntax
                 :syntax  {:num-objects 1
                           :loc1 #{:in-room :on-ground}}
                 :handler verbs/v-walk-around}

   ;; === Manipulation Verbs (continued) ===
   ;; ZIL: <SYNTAX MOVE OBJECT (ON-GROUND IN-ROOM) = V-MOVE PRE-MOVE>
   ;;      <SYNTAX PULL OBJECT (ON-GROUND IN-ROOM) = V-MOVE PRE-MOVE>
   ;;      <SYNTAX ROLL OBJECT (ON-GROUND IN-ROOM) = V-MOVE PRE-MOVE>
   :move       {:words   ["move" "pull" "roll" "push" "slide" "shift"]
                :syntax  {:num-objects 1
                          :loc1 #{:in-room :on-ground}}
                :handler verbs/v-move}})

;;; ---------------------------------------------------------------------------
;;; DIRECTION VOCABULARY
;;; ---------------------------------------------------------------------------
;;; Directions are a special part of speech. When the parser sees a bare
;;; direction like "north", it converts it to "walk north".
;;;
;;; ZIL: <DIRECTIONS NORTH EAST WEST SOUTH NE NW SE SW UP DOWN IN OUT LAND>

(def direction-definitions
  "Map of direction keywords to their vocabulary words.
   Each direction can have multiple synonyms."
  {:north ["north" "n"]
   :south ["south" "s"]
   :east  ["east" "e"]
   :west  ["west" "w"]
   :ne    ["northeast" "ne"]
   :nw    ["northwest" "nw"]
   :se    ["southeast" "se"]
   :sw    ["southwest" "sw"]
   :up    ["up" "u"]
   :down  ["down" "d"]
   :in    ["in" "inside"]
   :out   ["out" "outside" "exit" "leave"]
   :land  ["land"]})

(defn build-direction-vocabulary
  "Build vocabulary entries for directions.

   Returns a map of word-string -> {:parts-of-speech #{:direction} :dir-value dir-kw}"
  [definitions]
  (reduce-kv
   (fn [vocab dir-kw words]
     (reduce (fn [v word]
               (assoc v word {:parts-of-speech #{:direction}
                              :dir-value dir-kw}))
             vocab
             words))
   {}
   definitions))

(def direction-vocabulary
  "Vocabulary entries for direction words."
  (build-direction-vocabulary direction-definitions))

;;; ---------------------------------------------------------------------------
;;; PREPOSITION VOCABULARY
;;; ---------------------------------------------------------------------------
;;; Prepositions are used to modify verbs and connect objects.
;;; Examples: "put X IN Y", "walk AROUND house", "look UNDER bed"
;;;
;;; ZIL: Prepositions are defined with PR? constants and used in SYNTAX patterns.

(def preposition-definitions
  "Map of preposition keywords to their vocabulary words.
   These are used in syntax patterns like {:prep1 :around} to match
   'walk around house' → WALK AROUND OBJECT → v-walk-around"
  {:in      ["in" "into" "inside"]
   :on      ["on" "onto" "upon"]
   :with    ["with" "using"]
   :to      ["to" "toward" "towards"]
   :from    ["from"]
   :at      ["at"]
   :for     ["for"]
   :about   ["about"]
   :under   ["under" "underneath" "beneath"]
   :behind  ["behind"]
   :over    ["over"]
   :through ["through"]
   :around  ["around"]
   :off     ["off"]
   :out     ["out"]
   :up      ["up"]
   :down    ["down"]})

(defn build-preposition-vocabulary
  "Build vocabulary entries for prepositions.

   Returns a map of word-string -> {:parts-of-speech #{:preposition} :prep-value prep-kw}"
  [definitions]
  (reduce-kv
   (fn [vocab prep-kw words]
     (reduce (fn [v word]
               (update v word
                       (fn [existing]
                         (if existing
                           ;; Word exists - add preposition to its parts of speech
                           (-> existing
                               (update :parts-of-speech conj :preposition)
                               (assoc :prep-value prep-kw))
                           ;; New word
                           {:parts-of-speech #{:preposition}
                            :prep-value prep-kw}))))
             vocab
             words))
   {}
   definitions))

(def preposition-vocabulary
  "Vocabulary entries for preposition words."
  (build-preposition-vocabulary preposition-definitions))

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
  "Convert a syntax spec map to the internal syntax format.

   The :action field in the syntax spec can override the default action,
   allowing patterns like 'walk around X' to route to :walk-around instead of :walk."
  [action-kw {:keys [num-objects prep1 prep2 gwim1 gwim2 loc1 loc2 action]
              :or {num-objects 0}}]
  {:num-objects num-objects
   :prep1       prep1
   :prep2       prep2
   :gwim1       gwim1
   :gwim2       gwim2
   :loc1        (loc-set->bits loc1)
   :loc2        (loc-set->bits loc2)
   :action      (or action action-kw)})

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
  "Vocabulary entries for verbs. Merged with object/direction/preposition vocabulary."
  (merge (build-vocabulary verb-definitions)
         direction-vocabulary
         preposition-vocabulary))

(def ^:dynamic *verb-syntaxes*
  "Syntax patterns for each verb action."
  (build-verb-syntaxes verb-definitions))

(def ^:dynamic *verb-handlers*
  "Handler functions for each verb action."
  (build-verb-handlers verb-definitions))

;;; ---------------------------------------------------------------------------
;;; OBJECT VOCABULARY
;;; ---------------------------------------------------------------------------

(defn build-object-vocabulary
  "Build vocabulary entries from object definitions.

   Takes a sequence of objects (maps with :synonym and :adjective keys)
   and returns a vocabulary map where:
   - Each synonym word maps to {:parts-of-speech #{:object} :object-value word}
   - Each adjective word maps to {:parts-of-speech #{:adjective} :adj-value word}

   Words that appear as both synonym and adjective get merged parts-of-speech."
  [objects]
  (reduce
   (fn [vocab obj]
     (let [;; Process synonyms - can be vector of strings
           synonyms (or (:synonym obj) [])
           vocab-with-synonyms
           (reduce (fn [v word]
                     (let [word-lower (clojure.string/lower-case word)
                           existing (get v word-lower)
                           new-entry (if existing
                                       (-> existing
                                           (update :parts-of-speech conj :object)
                                           (assoc :object-value word-lower))
                                       {:parts-of-speech #{:object}
                                        :object-value word-lower})]
                       (assoc v word-lower new-entry)))
                   vocab
                   synonyms)

           ;; Process adjectives - can be string or vector of strings
           adjectives (let [adj (:adjective obj)]
                        (cond
                          (nil? adj) []
                          (string? adj) [adj]
                          (sequential? adj) adj
                          :else []))]
       (reduce (fn [v word]
                 (let [word-lower (clojure.string/lower-case word)
                       existing (get v word-lower)
                       new-entry (if existing
                                   (-> existing
                                       (update :parts-of-speech conj :adjective)
                                       (assoc :adj-value word-lower))
                                   {:parts-of-speech #{:adjective}
                                    :adj-value word-lower})]
                   (assoc v word-lower new-entry)))
               vocab-with-synonyms
               adjectives)))
   {}
   objects))

(defn register-object-vocabulary!
  "Register object vocabulary by merging it with the verb vocabulary.

   Call this after objects are added to the game state. Pass in the
   objects collection (values from game-state :objects).

   This updates the *verb-vocabulary* dynamic var in place, merging
   object words with existing verb definitions."
  [objects]
  (let [obj-vocab (build-object-vocabulary (vals objects))]
    (alter-var-root #'*verb-vocabulary*
                    (fn [current]
                      (merge-with
                       (fn [existing new]
                         ;; Merge parts-of-speech and keep all values
                         (-> existing
                             (update :parts-of-speech into (:parts-of-speech new))
                             (cond-> (:object-value new) (assoc :object-value (:object-value new)))
                             (cond-> (:adj-value new) (assoc :adj-value (:adj-value new)))))
                       current
                       obj-vocab)))))

;;; ---------------------------------------------------------------------------
;;; VERB DISPATCH
;;; ---------------------------------------------------------------------------

(defn- perform-single
  "Execute a verb action for a single object.
   Returns the updated game-state."
  [game-state handler]
  (let [prso (get-in game-state [:parser :prso])
        result-gs (handler game-state)]
    ;; Update :it to refer to the direct object (if any) for "it" pronoun
    (if-let [obj (first prso)]
      (utils/this-is-it result-gs obj)
      result-gs)))

(defn perform
  "Execute a verb action.

   ZIL: PERFORM routine in gmain.zil, MAIN-LOOP-1 multi-object loop

   When multiple direct objects are specified (e.g., 'take sword and lamp'),
   loops through each object, printing its name before executing the action.
   After successful execution with a direct object, updates :it to refer
   to that object for pronoun resolution.
   Returns the updated game-state."
  [game-state]
  (let [action (get-in game-state [:parser :prsa])
        prso (get-in game-state [:parser :prso])
        prsi (get-in game-state [:parser :prsi])
        ;; Get all objects - prso may be a vector of multiple objects
        all-prso (if (sequential? prso) prso (when prso [prso]))
        ;; Check if we're in "all" mode (even with single object)
        ;; ZIL: <OR <G? .NUM 1> <EQUAL? <GET <GET ,P-ITBL ,P-NC1> 0> ,W?ALL>>
        getflags (get-in game-state [:parser :getflags] 0)
        all-mode? (pos? (bit-and (or getflags 0) (:all game-state/getflags)))
        ;; Print prefixes when multiple objects OR in "all" mode
        multi? (and all-prso
                    (or (> (count all-prso) 1)
                        all-mode?))
        ;; Get handler for tracing
        handler (get *verb-handlers* action)
        handler-name (when handler (str action))
        ;; Trace verb dispatch if enabled
        gs (-> game-state
               (trace/trace-verb action prso prsi)
               (trace/trace-verb-dispatch action handler-name multi? all-mode?))]
    (if handler
      (if multi?
        ;; Multiple objects - loop through each one
        ;; ZIL: MAIN-LOOP-1 lines 99-153
        (reduce
         (fn [current-gs obj]
           ;; Print "object: " prefix
           (let [obj-name (game-state/thing-name current-gs obj)
                 ;; Trace individual object processing
                 gs-traced (trace/trace-verb-object current-gs obj obj-name)
                 gs-with-prefix (utils/tell gs-traced (str obj-name ": "))
                 ;; Set prso to just this single object for the handler
                 gs-single (assoc-in gs-with-prefix [:parser :prso] [obj])
                 result-gs (perform-single gs-single handler)]
             ;; Add newline after each object's output
             (utils/crlf result-gs)))
         gs
         all-prso)
        ;; Single object (or no object)
        (perform-single gs handler))
      (do
        (utils/tell gs (str "I don't know how to do that. [" action "]\n"))
        gs))))
