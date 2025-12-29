(ns clork.main-loop
  "Main game loop."
  (:require [clork.utils :as utils]
            [clork.parser :as parser]
            [clork.parser.input :as parser-input]
            [clork.verb-defs :as verb-defs]
            [clork.debug :as debug]
            [clork.undo :as undo]
            [clork.readline :as readline]
            [clork.daemon :as daemon]
            [clork.verbs-look :as verbs-look]
            [clork.verbs-meta :as verbs-meta]))

;;; ---------------------------------------------------------------------------
;;; MOVE COUNTING
;;; ---------------------------------------------------------------------------
;;; ZIL: Meta-verbs don't count as moves. See gmain.zil line ~150:
;;;   <COND (<VERB? TELL BRIEF SUPER-BRIEF VERBOSE SAVE VERSION
;;;                 QUIT RESTART SCORE SCRIPT UNSCRIPT RESTORE> T)
;;;         (T <SET V <CLOCKER>>)>>

(def ^:private meta-verbs
  "Verbs that don't count as moves (meta/system commands).
   ZIL: TELL BRIEF SUPER-BRIEF VERBOSE SAVE VERSION QUIT RESTART SCORE SCRIPT UNSCRIPT RESTORE
   Note: LOOK and INVENTORY are NOT meta-verbs - they count as moves and run daemons."
  #{:verbose :brief :super-brief :version :diagnose :score :quit :verify
    :restart :save :restore :script :unscript})

(defn- increment-moves-if-needed
  "Increment the move counter if the action is not a meta-verb.
   ZIL: MOVES global is only incremented for 'real' game actions."
  [game-state]
  (let [action (get-in game-state [:parser :prsa])]
    (if (contains? meta-verbs action)
      game-state
      (update game-state :moves (fnil inc 0)))))

;; Atom to track current game state for tab completion
(def ^:private current-state-atom (atom nil))

;; Wire up tab completion to access current game state
(readline/set-game-state-source! (fn [] @current-state-atom))

;; <ROUTINE MAIN-LOOP-1 ("AUX" ICNT OCNT NUM CNT OBJ TBL V PTBL OBJ1 TMP O I)
;;      <SET CNT 0>
;;      <SET OBJ <>>
;;      <SET PTBL T>
;;      <COND (<SETG P-WON <PARSER>>
;; 	    <SET ICNT <GET ,P-PRSI ,P-MATCHLEN>>
;; 	    <SET OCNT <GET ,P-PRSO ,P-MATCHLEN>>
;; 	    <COND (<AND ,P-IT-OBJECT <ACCESSIBLE? ,P-IT-OBJECT>>
;; 		   <SET TMP <>>
;; 		   <REPEAT ()
;; 			   <COND (<G? <SET CNT <+ .CNT 1>> .ICNT>
;; 				  <RETURN>)
;; 				 (T
;; 				  <COND (<EQUAL? <GET ,P-PRSI .CNT> ,IT>
;; 					 <PUT ,P-PRSI .CNT ,P-IT-OBJECT>
;; 					 <SET TMP T>
;; 					 <RETURN>)>)>>
;; 		   <COND (<NOT .TMP>
;; 			  <SET CNT 0>
;; 			  <REPEAT ()
;; 			   <COND (<G? <SET CNT <+ .CNT 1>> .OCNT>
;; 				  <RETURN>)
;; 				 (T
;; 				  <COND (<EQUAL? <GET ,P-PRSO .CNT> ,IT>
;; 					 <PUT ,P-PRSO .CNT ,P-IT-OBJECT>
;; 					 <RETURN>)>)>>)>
;; 		   <SET CNT 0>)>
;; 	    <SET NUM
;; 		 <COND (<0? .OCNT> .OCNT)
;; 		       (<G? .OCNT 1>
;; 			<SET TBL ,P-PRSO>
;; 			<COND (<0? .ICNT> <SET OBJ <>>)
;; 			      (T <SET OBJ <GET ,P-PRSI 1>>)>
;; 			.OCNT)
;; 		       (<G? .ICNT 1>
;; 			<SET PTBL <>>
;; 			<SET TBL ,P-PRSI>
;; 			<SET OBJ <GET ,P-PRSO 1>>
;; 			.ICNT)
;; 		       (T 1)>>
;; 	    <COND (<AND <NOT .OBJ> <1? .ICNT>> <SET OBJ <GET ,P-PRSI 1>>)>
;; 	    <COND (<AND <==? ,PRSA ,V?WALK>
;; 			<NOT <ZERO? ,P-WALK-DIR>>>
;; 		   <SET V <PERFORM ,PRSA ,PRSO>>)
;; 		  (<0? .NUM>
;; 		   <COND (<0? <BAND <GETB ,P-SYNTAX ,P-SBITS> ,P-SONUMS>>
;; 			  <SET V <PERFORM ,PRSA>>
;; 			  <SETG PRSO <>>)
;; 			 (<NOT ,LIT>
;; 			  <TELL "It's too dark to see." CR>)
;; 			 (T
;; 			  <TELL "It's not clear what you're referring to." CR>
;; 			  <SET V <>>)>)
;; 		  (T
;; 		   <SETG P-NOT-HERE 0>
;; 		   <SETG P-MULT <>>
;; 		   <COND (<G? .NUM 1> <SETG P-MULT T>)>
;; 		   <SET TMP <>>
;; 		   <REPEAT ()
;; 			   <COND (<G? <SET CNT <+ .CNT 1>> .NUM>
;; 				  <COND (<G? ,P-NOT-HERE 0>
;; 					 <TELL "The ">
;; 					 <COND (<NOT <EQUAL? ,P-NOT-HERE .NUM>>
;; 						<TELL "other ">)>
;; 					 <TELL "object">
;; 					 <COND (<NOT <EQUAL? ,P-NOT-HERE 1>>
;; 						<TELL "s">)>
;; 					 <TELL " that you mentioned ">
;; 					 <COND (<NOT <EQUAL? ,P-NOT-HERE 1>>
;; 						<TELL "are">)
;; 					       (T <TELL "is">)>
;; 					 <TELL "n't here." CR>)
;; 					(<NOT .TMP>
;; 					 <TELL
;; "There's nothing here you can take." CR>)>
;; 				  <RETURN>)
;; 				 (T
;; 				  <COND (.PTBL <SET OBJ1 <GET ,P-PRSO .CNT>>)
;; 					(T <SET OBJ1 <GET ,P-PRSI .CNT>>)>
;; 				  <SET O <COND (.PTBL .OBJ1) (T .OBJ)>>
;; 				  <SET I <COND (.PTBL .OBJ) (T .OBJ1)>>
;;
;; ;"multiple exceptions"
;; <COND (<OR <G? .NUM 1>
;; 	   <EQUAL? <GET <GET ,P-ITBL ,P-NC1> 0> ,W?ALL>>
;;        <SET V <LOC ,WINNER>>
;;        <COND (<EQUAL? .O ,NOT-HERE-OBJECT>
;; 	      <SETG P-NOT-HERE <+ ,P-NOT-HERE 1>>
;; 	      <AGAIN>)
;; 	     (<AND <VERB? TAKE>
;; 		   .I
;; 		   <EQUAL? <GET <GET ,P-ITBL ,P-NC1> 0> ,W?ALL>
;; 		   <NOT <IN? .O .I>>>
;; 	      <AGAIN>)
;; 	     (<AND <EQUAL? ,P-GETFLAGS ,P-ALL>
;; 		   <VERB? TAKE>
;; 		   <OR <AND <NOT <EQUAL? <LOC .O> ,WINNER ,HERE .V>>
;; 			    <NOT <EQUAL? <LOC .O> .I>>
;; 			    <NOT <FSET? <LOC .O> ,SURFACEBIT>>>
;; 		       <NOT <OR <FSET? .O ,TAKEBIT>
;; 				<FSET? .O ,TRYTAKEBIT>>>>>
;; 	      <AGAIN>)
;; 	     (ELSE
;; 	      <COND (<EQUAL? .OBJ1 ,IT>
;; 		     <PRINTD ,P-IT-OBJECT>)
;; 		    (T <PRINTD .OBJ1>)>
;; 	      <TELL ": ">)>)>
;; ;"end multiple exceptions"
;; 				  <SETG PRSO .O>
;; 				  <SETG PRSI .I>
;; 				  <SET TMP T>
;; 				  <SET V <PERFORM ,PRSA ,PRSO ,PRSI>>
;; 				  <COND (<==? .V ,M-FATAL> <RETURN>)>)>>)>
;; 	    <COND (<NOT <==? .V ,M-FATAL>>
;; 		   ;<COND (<==? <LOC ,WINNER> ,PRSO>
;; 			  <SETG PRSO <>>)>
;; 		   <SET V <APPLY <GETP <LOC ,WINNER> ,P?ACTION> ,M-END>>)>
;; 	    ;<COND (<VERB? ;AGAIN ;"WALK -- why was this here?"
;; 			  SAVE RESTORE ;SCORE ;VERSION ;WAIT> T)
;; 		  (T
;; 		   <SETG L-PRSA ,PRSA>
;; 		   <SETG L-PRSO ,PRSO>
;; 		   <SETG L-PRSI ,PRSI>)>
;; 	    <COND (<==? .V ,M-FATAL> <SETG P-CONT <>>)>)
;; 	   (T
;; 	    <SETG P-CONT <>>)>
;;      %<COND (<==? ,ZORK-NUMBER 3>
;; 	     '<COND (<NOT ,CLEFT-QUEUED?>
;; 		     <ENABLE <QUEUE I-CLEFT <+ 70 <RANDOM 70>>>>
;; 		     <SETG CLEFT-QUEUED? T>)>)
;; 	    (ELSE '<NULL-F>)>
;;      <COND (,P-WON
;; 	    <COND (<VERB? TELL BRIEF SUPER-BRIEF VERBOSE SAVE VERSION
;; 			  QUIT RESTART SCORE SCRIPT UNSCRIPT RESTORE> T)
;; 		  (T <SET V <CLOCKER>>)>)>>

(defn- skip-input-line?
  "Check if a line should be skipped (comment or blank).
   Comments start with ; or # after trimming whitespace."
  [s]
  (when s
    (let [trimmed (clojure.string/trim s)]
      (or (empty? trimmed)
          (clojure.string/starts-with? trimmed ";")
          (clojure.string/starts-with? trimmed "#")))))

(defn- read-input-phase
  "Read input from player, handling reserve/cont buffers.
   Returns game-state with :input populated.
   Comments (lines starting with ; or #) and blank lines are skipped."
  [game-state]
  (loop [gs (-> game-state
                (parser/parser-init)
                (parser-input/parser-set-winner-to-player)
                (parser-input/parser-read-command))]
    (if (skip-input-line? (:input gs))
      ;; Skip comment/blank, read next line
      (recur (parser-input/parser-read-command gs))
      gs)))

(defn main-loop-once
  "Execute one iteration of the main loop.

   ZIL: MAIN-LOOP-1 in gmain.zil

   This function:
   1. Reads input from the player
   2. Pushes state to undo stack (for non-debug commands)
   3. Checks for $ debug commands (intercepted early)
   4. If normal command, calls the parser
   5. If parsing succeeded, calls perform to execute the action
   6. Returns the updated game-state"
  [game-state]
  ;; Update state atom for tab completion before reading input
  (reset! current-state-atom game-state)
  ;; Phase 1: Read input
  (let [gs (read-input-phase game-state)
        input (:input gs)]
    ;; Handle EOF (nil input)
    (if (nil? input)
      (assoc gs :quit true)
      ;; Phase 2: Check for $ debug command
      (if (debug/debug-command? input)
        ;; Handle debug command - bypass normal parser
        ;; Don't push to undo stack for debug commands
        ;; Debug commands don't count as moves
        (-> gs
            (debug/dispatch input)
            (utils/crlf))
        ;; Phase 3: Normal parsing
        ;; Push to undo stack BEFORE modifying state
        (let [gs (undo/push-undo gs input)
              gs (parser/parser-from-input gs)]
          (if (parser/get-parser-error gs)
            ;; Parsing failed - error already displayed by parser
            gs
            ;; Parsing succeeded - perform the action, then run daemons
            (-> gs
                (verb-defs/perform)
                (increment-moves-if-needed)
                (daemon/clocker)
                (utils/crlf))))))))

(defn- max-turns-exceeded?
  "Check if max turns has been exceeded (for script mode)."
  [game-state]
  (when-let [max-turns (get-in game-state [:script-config :max-turns])]
    (>= (:moves game-state 0) max-turns)))

(defn main-loop
  "The main loop for the game."
  [game-state]
  (loop [gs game-state]
    (cond
      ;; Normal quit
      (:quit gs)
      gs

      ;; Restart - restore from initial state
      ;; ZIL: <RESTART> opcode resets game to initial state
      (:restart gs)
      (if-let [restart-state (:restart-state gs)]
        ;; Restore initial state, show version and look
        (let [restarted (-> restart-state
                            (verbs-meta/v-version)
                            (utils/crlf)
                            (verbs-look/v-look))]
          (recur restarted))
        ;; No restart-state available (shouldn't happen)
        (do
          (utils/tell gs "\nRestart failed.")
          (recur (dissoc gs :restart))))

      ;; Restored from save file - show room
      ;; ZIL: V-RESTORE calls V-FIRST-LOOK after successful restore
      (:restored gs)
      (let [looked (-> gs
                       (dissoc :restored)
                       (verbs-look/v-look))]
        (recur looked))

      ;; Max turns exceeded (script mode)
      (max-turns-exceeded? gs)
      (-> gs
          (utils/tell "\n[Max turns exceeded]\n")
          (assoc :quit true :max-turns-exceeded true))

      ;; Continue looping
      :else
      (recur (main-loop-once gs)))))