(ns clork.parser.input
  "Reading and managing input for the parser.

   This module handles:
   - Reading player input from the terminal
   - Managing lexical value tables (lexv)
   - Handling AGAIN, OOPS, and multi-command input
   - Table copying utilities (STUFF, INBUF-STUFF, INBUF-ADD)

   ZIL Reference: gparser.zil
   - Lines 131-153: Input source selection in PARSER routine
   - Lines 386-423: STUFF, INBUF-STUFF, INBUF-ADD routines

   Data Flow:
     Terminal Input → lexv table → parsing pipeline
     For AGAIN: again-lexv → lexv
     For multi-command: reserve-lexv → lexv (when first command done)

   Key Concepts:
   - lexv: The 'lexical value' table containing tokenized input
   - reserve-lexv: Holds overflow when player types multiple commands
   - again-lexv: Holds previous command for AGAIN
   - P-CONT: Pointer to next command in a 'then' chain"
  (:require [clork.utils :as utils]
            [clork.game-state :as game-state]
            [clork.parser.validation :as validation]))

;;; ---------------------------------------------------------------------------
;;; INPUT SOURCE SELECTION
;;; ---------------------------------------------------------------------------
;;; The parser can get input from three sources:
;;;   1. reserve-lexv - Buffered second command from "go north then east"
;;;   2. P-CONT - Continuation within current lexv for chained commands
;;;   3. Fresh input - Read new line from player
;;;
;;; Priority: reserve-ptr > cont > new input

(declare parser-set-here-to-winner-loc)

(defn parser-restore-reserve
  "Restore a reserved command from the reserve buffer.

   When the player types 'go north then east', the 'east' part gets
   stored in reserve-lexv. After 'go north' completes, this function
   restores 'east' as the next command.

   ZIL: Lines 131-137 of PARSER routine
     (,RESERVE-PTR
       <SET PTR ,RESERVE-PTR>
       <STUFF ,RESERVE-LEXV ,P-LEXV>
       <COND (<AND <NOT ,SUPER-BRIEF> <EQUAL? ,PLAYER ,WINNER>>
         <CRLF>)>
       <SETG RESERVE-PTR <>>
       <SETG P-CONT <>>)"
  [game-state]
  (let [winner-is-player? (= (:winner game-state) (:player game-state))
        is-not-super-brief? (not (:super-brief game-state))]
    (-> game-state
        ;; <SET PTR ,RESERVE-PTR>
        (assoc-in [:parser :ptr] (get-in game-state [:parser :reserve-ptr]))
        ;; <STUFF ,RESERVE-LEXV ,P-LEXV> - copy reserve to main lexv
        (assoc-in [:parser :lexv] (get-in game-state [:parser :reserve-lexv]))
        ;; Print newline if appropriate
        (utils/crlf-if (and is-not-super-brief? winner-is-player?))
        ;; <SETG RESERVE-PTR <>>
        (assoc-in [:parser :reserve-ptr] nil)
        ;; <SETG P-CONT <>>
        (assoc-in [:parser :cont] nil))))

(defn parser-restore-cont
  "Continue parsing at the next command in a 'then' chain.

   For input like 'take lamp then go north', after 'take lamp' completes,
   P-CONT points to 'go north' within the same lexv buffer.

   ZIL: Lines 138-144 of PARSER routine
     (,P-CONT
       <SET PTR ,P-CONT>
       <COND (<AND <NOT ,SUPER-BRIEF>
           <EQUAL? ,PLAYER ,WINNER>
           <NOT <VERB? SAY>>>
         <CRLF>)>
       <SETG P-CONT <>>)"
  [game-state]
  (let [winner-is-player? (= (:winner game-state) (:player game-state))
        is-not-super-brief? (not (:super-brief game-state))
        verb-is-not-say? (not= :say (get-in game-state [:parser :verb]))]
    (-> game-state
        ;; <SET PTR ,P-CONT>
        (assoc-in [:parser :ptr] (get-in game-state [:parser :cont]))
        ;; Print newline if appropriate (not during SAY to preserve quoting)
        (utils/crlf-if (and is-not-super-brief? winner-is-player? verb-is-not-say?))
        ;; <SETG P-CONT <>>
        (assoc-in [:parser :cont] nil))))

(defn parser-read-command-input
  "Read a fresh command from the player.

   This is the normal case when there's no buffered or continued command.
   Prints the prompt, reads a line, and stores it for parsing.

   ZIL: Lines 145-153 of PARSER routine
     (T
       <SETG WINNER ,PLAYER>
       <SETG QUOTE-FLAG <>>
       <COND (<NOT <FSET? <LOC ,WINNER> ,VEHBIT>>
         <SETG HERE <LOC ,WINNER>>)>
       <SETG LIT <LIT? ,HERE>>
       <COND (<NOT ,SUPER-BRIEF> <CRLF>)>
       <TELL \">\">
       <READ ,P-INBUF ,P-LEXV>)"
  [game-state]
  (-> game-state
      ;; Reset winner to player (not an NPC from SAY command)
      ;; <SETG WINNER ,PLAYER>
      (assoc :winner (:player game-state))
      ;; <SETG QUOTE-FLAG <>>
      (assoc-in [:parser :quote-flag] false)
      ;; <COND (<NOT <FSET? <LOC ,WINNER> ,VEHBIT>> ...)>
      ;; Update HERE unless player is in a vehicle
      (parser-set-here-to-winner-loc)
      ;; <SETG LIT <LIT? ,HERE>>
      ;; Update lighting status
      ((fn [gs] (assoc gs :lit (validation/lit? gs (:here gs)))))
      ;; <COND (<NOT ,SUPER-BRIEF> <CRLF>)>
      ((fn [gs] (utils/crlf-if gs (not (:super-brief gs)))))
      ;; <TELL ">">
      (utils/tell ">")
      ;; <READ ,P-INBUF ,P-LEXV>
      ;; Read input and store in :input (will be tokenized by lexer)
      ((fn [gs] (assoc gs :input (read-line))))))

(defn parser-set-here-to-winner-loc
  "Update HERE to the winner's location, unless they're in a vehicle.

   Vehicles have their own location tracking, so we don't update HERE
   when the player is in one.

   ZIL: Lines 148-149 of PARSER routine (also used in set-winner-to-player)
     <COND (<NOT <FSET? <LOC ,WINNER> ,VEHBIT>>
       <SETG HERE <LOC ,WINNER>>)>"
  [game-state]
  (if-let [winner (game-state/get-thing game-state (:winner game-state))]
    (let [winner-loc (:in winner)]
      ;; Only update HERE if winner has a location and it's not a vehicle
      (if (and winner-loc
               (not (game-state/set-thing-flag? game-state winner-loc :vehicle)))
        (assoc game-state :here winner-loc)
        game-state))
    game-state))

(defn parser-read-command
  "Get the next command from the appropriate source.

   Dispatches to one of three sources based on parser state:
   1. Reserve buffer (second command from 'X then Y')
   2. Continuation pointer (next clause in chained input)
   3. Fresh input from terminal

   ZIL: The COND at lines 131-153 of PARSER routine"
  [game-state]
  (cond
    ;; Priority 1: Restore buffered second command
    (get-in game-state [:parser :reserve-ptr])
    (parser-restore-reserve game-state)

    ;; Priority 2: Continue with next command in chain
    (get-in game-state [:parser :cont])
    (parser-restore-cont game-state)

    ;; Priority 3: Read fresh input
    :else
    (parser-read-command-input game-state)))

;;; ---------------------------------------------------------------------------
;;; WINNER/PLAYER MANAGEMENT
;;; ---------------------------------------------------------------------------

(defn parser-set-winner-to-player
  "Reset WINNER to PLAYER when not in quoted speech mode.

   During SAY commands, WINNER temporarily becomes the NPC being spoken to.
   This resets it back to the player for normal commands.

   ZIL: Lines 125-130 of PARSER routine
     <COND (<AND <NOT ,QUOTE-FLAG> <N==? ,WINNER ,PLAYER>>
       <SETG WINNER ,PLAYER>
       <SETG HERE <META-LOC ,PLAYER>>
       <SETG LIT <LIT? ,HERE>>)>"
  [game-state]
  (let [in-quotes? (get-in game-state [:parser :quote-flag])
        winner (:winner game-state)
        player (:player game-state)
        should-reset? (and (not in-quotes?)
                           (not= winner player))]
    (if should-reset?
      (-> game-state
          (assoc :winner player)
          ;; META-LOC finds the room containing an object
          (assoc :here (validation/meta-loc game-state player))
          ((fn [gs] (assoc gs :lit (validation/lit? gs (:here gs))))))
      game-state)))

;;; ---------------------------------------------------------------------------
;;; TABLE COPY UTILITIES
;;; ---------------------------------------------------------------------------
;;; These functions copy lexical tables for AGAIN/OOPS functionality.
;;; In ZIL these do byte-level copying; in Clojure we can just copy vectors.

;; TODO: Implement when we have the lexv structure defined
;;
;; ZIL: <ROUTINE STUFF (SRC DEST ...)> - Lines 386-399
;; Copies a lexv table from SRC to DEST. Used for AGAIN command.
;;
;; ZIL: <ROUTINE INBUF-STUFF (SRC DEST ...)> - Lines 401-406
;; Copies input buffer contents. Used with OOPS and AGAIN.
;;
;; ZIL: <ROUTINE INBUF-ADD (LEN BEG SLOT ...)> - Lines 408-423
;; Adds a word to OOPS-INBUF for error correction.

(defn stuff
  "Copy a lexv table from source to destination.

   ZIL: STUFF routine, lines 386-399
   For AGAIN purposes, put contents of one LEXV table into another.

   In Clojure, since data is immutable, this just returns the source
   value to be assoc'd into the destination location."
  [source]
  source)

(defn inbuf-stuff
  "Copy input buffer contents from source to destination.

   ZIL: INBUF-STUFF routine, lines 401-406
   Used to preserve raw input for OOPS corrections."
  [source]
  source)
