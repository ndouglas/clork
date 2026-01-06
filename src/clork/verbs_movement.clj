(ns clork.verbs-movement
  "Movement handlers: walk, through, walk-around, move, climb.

   ZIL Reference: V-WALK, V-THROUGH, V-WALK-AROUND, V-MOVE, V-CLIMB-* in gverbs.zil."
  (:require [clork.utils :as utils]
            [clork.game-state :as gs]
            [clork.parser.state :as parser-state]
            [clork.verbs-look :as verbs-look]
            [clork.verbs-health :as verbs-health]
            [clork.verbs-inventory :as verbs-inv]
            [clork.debug.trace :as trace]))

;;; ---------------------------------------------------------------------------
;;; SPECIAL EXIT FUNCTIONS (PER routines)
;;; ---------------------------------------------------------------------------

(defn maze-diodes
  "One-way passages in the maze. Print warning and return destination.

   ZIL: MAZE-DIODES routine in 1actions.zil (lines 911-918)
   These are chutes that you can go down but not back up."
  [game-state]
  (let [here (:here game-state)
        destination (case here
                      :maze-2 :maze-4
                      :maze-7 :dead-end-1
                      :maze-9 :maze-11
                      :maze-12 :maze-5
                      nil)]
    (when destination
      {:destination destination
       :message "You won't be able to get back up to the tunnel you are going through when it gets to the next room."})))

(defn grating-exit
  "Exit through the grating in GRATING-CLEARING.

   ZIL: GRATING-EXIT routine in 1dungeon.zil (lines 1400-1408)

   - If grate not revealed: \"You can't go that way.\"
   - If grate revealed but closed: \"The grating is closed!\" + set IT to grate
   - If grate revealed and open: move to GRATING-ROOM"
  [game-state]
  (let [grate-revealed? (get game-state :grate-revealed false)
        grate-open? (gs/set-thing-flag? game-state :grate :open)]
    (cond
      ;; Grate not revealed - hidden under leaves
      (not grate-revealed?)
      {:blocked true
       :message "You can't go that way."}

      ;; Grate revealed but closed
      (not grate-open?)
      {:blocked true
       :message "The grating is closed!"
       :set-it :grate}

      ;; Grate revealed and open - can go through
      :else
      {:destination :grating-room})))

(defn up-chimney-function
  "Exit up the chimney from the studio to the kitchen.

   ZIL: UP-CHIMNEY-FUNCTION routine in 1actions.zil (lines 566-578)

   Conditions to climb:
   - Must not be empty-handed (need something to hold onto)
   - Can only carry 1-2 items, and must have the lamp
   - If trap door is closed, clear TOUCHBIT (allows it to close again on next descent)

   Returns {:destination :kitchen} on success, {:blocked true :message ...} on failure."
  [game-state]
  (let [winner (:winner game-state)
        contents (gs/get-contents game-state winner)
        item-count (count contents)
        has-lamp? (some #{:brass-lantern} contents)]
    (cond
      ;; Empty-handed
      (zero? item-count)
      {:blocked true
       :message "Going up empty-handed is a bad idea."}

      ;; Too many items (more than 2, or 2 without the lamp)
      (or (> item-count 2)
          (and (= item-count 2) (not has-lamp?)))
      {:blocked true
       :message "You can't get up there with what you're carrying."}

      ;; Must have the lamp
      (not has-lamp?)
      {:blocked true
       :message "You can't get up there with what you're carrying."}

      ;; Success - can climb up
      ;; ZIL: If trap door is closed, clear TOUCHBIT so it can close again
      :else
      (let [trap-door-open? (gs/set-thing-flag? game-state :trap-door :open)]
        {:destination :kitchen
         :side-effect (when (not trap-door-open?)
                        ;; Clear TOUCHBIT so trap door can close again on next descent
                        (fn [gs] (gs/unset-thing-flag gs :trap-door :touch)))}))))

(def per-functions
  "Map of :per function names to their implementations."
  {:maze-diodes maze-diodes
   :grating-exit grating-exit
   :up-chimney-function up-chimney-function})

;;; ---------------------------------------------------------------------------
;;; MOVEMENT HELPERS
;;; ---------------------------------------------------------------------------

(defn get-exit
  "Get the exit definition for a direction from the current room.
   Returns nil if no exit, a keyword for room destination, or a string for blocked message."
  [game-state direction]
  (let [here (:here game-state)
        room (gs/get-thing game-state here)]
    (get-in room [:exits direction])))

(defn room-lit?
  "Check if a room is lit.

   Delegates to validation/lit? which does a complete check including:
   1. Room's :lit flag
   2. Light sources carried by winner/player
   3. Light sources in the room (including inside containers)

   Uses requiring-resolve to avoid circular dependency.

   ZIL Reference: LIT? routine in gparser.zil lines 1333-1355"
  [game-state room-id]
  (let [lit-fn (requiring-resolve 'clork.parser.validation/lit?)]
    (lit-fn game-state room-id)))

(defn goto
  "Move the player to a new room and describe it.

   ZIL: GOTO routine in gverbs.zil (lines 2061-2110)
   - Move winner to new room
   - Update HERE global
   - Update LIT flag
   - Score the room (if it has a value)
   - Call room action with M-ENTER
   - Call V-FIRST-LOOK to describe room

   Returns updated game-state."
  [game-state room-id]
  (let [gs (trace/trace-enter game-state :verbs "goto" {:room room-id})
        winner (:winner gs)
        ;; Move the winner to the new room
        gs (assoc-in gs [:objects winner :in] room-id)
        ;; Update HERE
        gs (assoc gs :here room-id)
        ;; Update LIT flag
        gs (assoc gs :lit (room-lit? gs room-id))
        ;; Score the room (ZIL: SCORE-OBJ .RM in gverbs.zil line 2138)
        ;; Rooms can have a :value property for first-time entry points
        room (gs/get-thing gs room-id)
        room-value (get room :value 0)
        gs (if (pos? room-value)
             (-> gs
                 (verbs-health/score-upd room-value)
                 (assoc-in [:rooms room-id :value] 0))
             gs)
        ;; Call room action with M-ENTER (ZIL: room actions get called on entry)
        ;; Room action may return game-state with :use-default-handling flag
        room-action (:action room)
        gs (if room-action
             (let [result (room-action gs :m-enter)]
               (if (gs/use-default? result)
                 (gs/clear-use-default result)
                 result))
             gs)]
    ;; Describe the room (V-FIRST-LOOK)
    ;; ZIL: V-FIRST-LOOK only calls DESCRIBE-OBJECTS if room is lit
    (-> gs
        (verbs-look/v-first-look)
        (trace/trace-return :verbs "goto"))))

;;; ---------------------------------------------------------------------------
;;; GRUE ATTACK
;;; ---------------------------------------------------------------------------
;;; ZIL: The grue attacks when the player tries to walk in darkness.
;;; See gverbs.zil lines 1593-1594:
;;;   <JIGS-UP "Oh, no! You have walked into the slavering fangs of a lurking grue!">

(defn- grue-attack
  "Kill the player with a grue attack.

   ZIL: Called when player walks in darkness (gverbs.zil line 1594).
   Uses jigs-up from death module (via requiring-resolve to avoid circular deps)."
  [game-state]
  (let [jigs-up (requiring-resolve 'clork.death/jigs-up)]
    (jigs-up game-state
             "Oh, no! You have walked into the slavering fangs of a lurking grue!")))

;;; ---------------------------------------------------------------------------
;;; WALK COMMAND
;;; ---------------------------------------------------------------------------
;;; ZIL: V-WALK, DO-WALK in gverbs.zil

(defn v-walk
  "Move in a direction.

   ZIL: V-WALK in gverbs.zil (lines 1537-1597)
   Handles various exit types:
   - Simple exit: (NORTH TO KITCHEN) -> move to room
   - Blocked exit: (EAST \"The door is boarded.\") -> print message
   - Conditional exit: (SW TO BARROW IF WON-FLAG) -> check flag
   - Door exit: (WEST TO ROOM IF DOOR IS OPEN) -> check door

   IMPORTANT: If the room is dark, attempting to walk triggers a grue attack!"
  [game-state]
  (let [;; Get direction from parser (stored in prso as a keyword)
        prso (parser-state/get-prso game-state)
        direction (if (keyword? prso) prso prso)
        ;; Check if current room is lit
        here (:here game-state)
        lit? (room-lit? game-state here)]
    (cond
      ;; No direction specified
      (nil? direction)
      (utils/tell game-state "You must specify a direction to go.")

      ;; ZIL: Walking in darkness triggers grue attack (gverbs.zil line 1593-1594)
      ;; <NOT ,LIT> -> <JIGS-UP "Oh, no! You have walked into the slavering fangs of a lurking grue!">
      (not lit?)
      (grue-attack game-state)

      :else
      (let [exit (get-exit game-state direction)]
        (cond
          ;; No exit in that direction
          (nil? exit)
          (utils/tell game-state "You can't go that way.")

          ;; Exit is a string (blocked message)
          (string? exit)
          (utils/tell game-state exit)

          ;; Exit is a map with conditions
          (map? exit)
          (let [{:keys [to if door per]} exit
                door-obj (when door (gs/get-thing game-state door))]
            (cond
              ;; PER function - special exit handler (like MAZE-DIODES, GRATING-EXIT)
              per
              (if-let [per-fn (get per-functions per)]
                (if-let [result (per-fn game-state)]
                  (cond
                    ;; Blocked - just print message
                    (:blocked result)
                    (let [gs (if (:message result)
                               (utils/tell game-state (:message result))
                               game-state)]
                      ;; ZIL: THIS-IS-IT sets the pronoun reference
                      (if (:set-it result)
                        (assoc gs :it (:set-it result))
                        gs))

                    ;; Can go through - optionally print message, apply side-effect, then move
                    (:destination result)
                    (let [gs (if (:message result)
                               (-> game-state
                                   (utils/tell (:message result))
                                   (utils/crlf))
                               game-state)
                          ;; Apply side-effect function if present (e.g., clear TOUCHBIT)
                          gs (if-let [side-effect (:side-effect result)]
                               (side-effect gs)
                               gs)]
                      (goto gs (:destination result)))

                    ;; Unknown result
                    :else
                    (utils/tell game-state "You can't go that way."))
                  (utils/tell game-state "You can't go that way."))
                (utils/tell game-state "You can't go that way."))

              ;; Conditional on a flag
              (and if (not (get game-state if)))
              (if-let [else-msg (:else exit)]
                (utils/tell game-state else-msg)
                (utils/tell game-state "You can't go that way."))

              ;; Door is invisible (hidden) - can't go that way
              (and door (gs/set-thing-flag? game-state door :invisible))
              (utils/tell game-state "You can't go that way.")

              ;; Conditional on a door being open
              (and door (not (gs/set-thing-flag? game-state door :open)))
              (let [door-desc (:desc door-obj)]
                (utils/tell game-state (str "The " door-desc " is closed.")))

              ;; All conditions met - go to the room
              :else
              (goto game-state to)))

          ;; Exit is a keyword (simple room destination)
          (keyword? exit)
          (goto game-state exit)

          ;; Unknown exit type
          :else
          (utils/tell game-state "You can't go that way."))))))

;;; ---------------------------------------------------------------------------
;;; ENTER COMMAND
;;; ---------------------------------------------------------------------------
;;; ZIL: V-ENTER in gverbs.zil

(defn v-enter
  "Enter (bare command) - walk in the :in direction.

   ZIL: V-ENTER in gverbs.zil (lines 636-637):
   <ROUTINE V-ENTER ()
     <DO-WALK ,P?IN>>

   Simply walks in the :in direction, which is typically used to enter
   buildings, vehicles, etc."
  [game-state]
  ;; Set the direction to :in and call v-walk
  (let [gs (assoc-in game-state [:parser :prso] [:in])]
    (v-walk gs)))

;;; ---------------------------------------------------------------------------
;;; THROUGH COMMAND
;;; ---------------------------------------------------------------------------
;;; ZIL: V-THROUGH in gverbs.zil

(defn v-through
  "Go through a door, window, or other passageway.

   ZIL: V-THROUGH in gverbs.zil

   First tries the object's action handler. If the object handles the verb,
   we're done. Otherwise, if the object has :door flag, try to walk through
   to the other side."
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        obj (gs/get-thing game-state prso)
        desc (:desc obj)
        flags (or (:flags obj) #{})
        action-fn (:action obj)]
    ;; First try the object's action handler
    (if-let [result (when action-fn (action-fn game-state))]
      ;; Object handled the verb
      result
      ;; Object didn't handle it - try default door behavior
      (cond
        ;; Object has door flag - find the other side and walk there
        (contains? flags :door)
        (let [here (:here game-state)
              ;; Find which room has an exit through this door
              rooms (:rooms game-state)
              ;; Look for exits that reference this door
              other-side (some (fn [[room-id room]]
                                 (when (not= room-id here)
                                   (some (fn [[dir exit]]
                                           (when (and (map? exit)
                                                      (= (:door exit) prso))
                                             (:to exit)))
                                         (:exits room))))
                               rooms)]
          (if (and other-side (contains? flags :open))
            (goto game-state other-side)
            (if (not (contains? flags :open))
              (utils/tell game-state (str "The " desc " is closed."))
              (utils/tell game-state "You can't go that way."))))

        ;; Not a door - can't go through it
        :else
        (utils/tell game-state "I don't know how to do that.")))))

;;; ---------------------------------------------------------------------------
;;; WALK-AROUND COMMAND
;;; ---------------------------------------------------------------------------
;;; ZIL: V-WALK-AROUND in gverbs.zil

(defn v-walk-around
  "Walk around an object.

   ZIL: V-WALK-AROUND in gverbs.zil

   First tries the object's action handler. If the object handles the verb
   (like the white house cycling through exterior rooms), we're done.
   Otherwise, just tell player to use compass directions."
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        obj (gs/get-thing game-state prso)
        action-fn (:action obj)]
    ;; First try the object's action handler
    (if-let [result (when action-fn (action-fn game-state))]
      ;; Object handled the verb
      result
      ;; Object didn't handle it - default message
      (utils/tell game-state "Use compass directions for movement."))))

;;; ---------------------------------------------------------------------------
;;; MOVE COMMAND
;;; ---------------------------------------------------------------------------
;;; ZIL: V-MOVE and PRE-MOVE in gverbs.zil

(defn v-move
  "Move an object.

   ZIL: V-MOVE in gverbs.zil (line 930)

   First tries the object's action handler (e.g., the rug reveals the trap door).
   If not handled:
   - If player is holding it: \"You aren't an accomplished enough juggler.\"
   - If takeable: \"Moving the X reveals nothing.\"
   - Otherwise: \"You can't move the X.\""
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        obj (gs/get-thing game-state prso)
        desc (:desc obj)
        flags (or (:flags obj) #{})
        action-fn (:action obj)]
    ;; First try the object's action handler
    (if-let [result (when action-fn (action-fn game-state))]
      ;; Object handled the verb
      result
      ;; Object didn't handle it - default behavior
      (cond
        ;; PRE-MOVE: Can't move something you're holding
        (verbs-inv/already-holding? game-state prso)
        (utils/tell game-state "You aren't an accomplished enough juggler.")

        ;; Takeable object - moving reveals nothing
        (contains? flags :take)
        (utils/tell game-state (str "Moving the " desc " reveals nothing."))

        ;; Can't move it
        :else
        (utils/tell game-state (str "You can't move the " desc "."))))))

;;; ---------------------------------------------------------------------------
;;; CLIMB COMMANDS
;;; ---------------------------------------------------------------------------
;;; ZIL: V-CLIMB-UP, V-CLIMB-DOWN, V-CLIMB-FOO, V-CLIMB-ON in gverbs.zil

(defn- climbable?
  "Returns true if the object has the :climb flag.

   ZIL: CLIMBBIT flag - objects like trees, stairs, ladders, etc."
  [obj]
  (contains? (or (:flags obj) #{}) :climb))

(defn- has-exit?
  "Returns true if the current room has an exit in the given direction."
  [game-state direction]
  (let [exit (get-exit game-state direction)]
    (some? exit)))

(defn v-climb-up
  "Climb up (or down, depending on direction passed).

   ZIL: V-CLIMB-UP in gverbs.zil (line 316)

   This is the core climb routine. It:
   1. Checks if there's an exit in the given direction
   2. If climbing a specific object, verifies it leads that way
   3. Performs the walk

   Called directly for 'climb up X' or via v-climb-down/v-climb-foo.

   When PRSO is :rooms (from RMUNGBIT via GWIM), it means no specific object
   was named - just 'climb up' or 'climb down'. In this case we try to walk
   in the direction."
  ([game-state] (v-climb-up game-state :up))
  ([game-state direction]
   (let [prso (parser-state/get-prso game-state)
         ;; ZIL: <COND (<AND .OBJ <NOT <EQUAL? ,PRSO ,ROOMS>>> <SET OBJ ,PRSO>)>
         ;; When PRSO is ROOMS (our :rooms), treat it as no object
         prso (when (not= prso :rooms) prso)
         obj (when prso (gs/get-thing game-state prso))
         desc (when obj (:desc obj))
         ;; Check if there's an exit in the requested direction
         exit (get-exit game-state direction)
         dir-word (if (= direction :up) "upward" "downward")]
     ;; First try the object's action handler if there is one
     (if-let [result (when (and obj (:action obj))
                       ((:action obj) game-state))]
       result
       ;; Default climb behavior
       (cond
         ;; There's an exit - walk that direction
         (and exit (not (string? exit)))
         (-> game-state
             (assoc-in [:parser :prso] [direction])
             v-walk)

         ;; Exit is blocked with a message (like "You cannot climb any higher.")
         (string? exit)
         (utils/tell game-state exit)

         ;; Climbing a wall-like object
         (and obj
              (let [synonyms (or (:synonym obj) [])]
                (some #{"wall" "walls"} synonyms)))
         (utils/tell game-state "Climbing the walls is to no avail.")

         ;; Climbing a specific object but no exit that way
         (and obj (climbable? obj))
         (utils/tell game-state
                     (str "The " desc
                          (if (= prso :stairs) " don't" " doesn't")
                          " lead " dir-word "."))

         ;; No object specified (or :rooms), no exit
         ;; ZIL: (<EQUAL? .OBJ <> ,ROOMS> <TELL "You can't go that way." CR>)
         (nil? obj)
         (utils/tell game-state "You can't go that way.")

         ;; Trying to climb something not climbable
         :else
         (utils/tell game-state "You can't do that!"))))))

(defn v-climb-down
  "Climb down.

   ZIL: V-CLIMB-DOWN in gverbs.zil (line 295)
     <ROUTINE V-CLIMB-DOWN () <V-CLIMB-UP ,P?DOWN ,PRSO>>

   Just calls v-climb-up with direction :down."
  [game-state]
  (v-climb-up game-state :down))

(defn v-climb-foo
  "Climb something (direction unspecified, defaults to up).

   ZIL: V-CLIMB-FOO in gverbs.zil (line 297)
     In Zork I: <V-CLIMB-UP ,P?UP ,PRSO>

   When you just say 'climb tree' without specifying up or down,
   this defaults to climbing up."
  [game-state]
  (v-climb-up game-state :up))

(defn v-climb-on
  "Climb onto an object.

   ZIL: V-CLIMB-ON in gverbs.zil (line 306)

   If the object is a vehicle (VEHBIT), board it.
   Otherwise, print an error message."
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        obj (gs/get-thing game-state prso)
        desc (:desc obj)
        flags (or (:flags obj) #{})]
    ;; First try the object's action handler
    (if-let [result (when (:action obj) ((:action obj) game-state))]
      result
      ;; Default behavior
      (cond
        ;; Vehicle - board it (not implemented yet, so just acknowledge)
        (contains? flags :vehicle)
        (utils/tell game-state (str "You are now in the " desc "."))

        ;; Not a vehicle - can't climb onto it
        :else
        (utils/tell game-state (str "You can't climb onto the " desc "."))))))

;;; ---------------------------------------------------------------------------
;;; BACK COMMAND
;;; ---------------------------------------------------------------------------
;;; ZIL: V-BACK in gverbs.zil

(defn v-back
  "Go back to the previous room.

   ZIL: V-BACK in gverbs.zil (line 211)
     <ROUTINE V-BACK ()
       <TELL \"Sorry, my memory is poor. Please give a direction.\" CR>>

   Note: The original Zork I doesn't actually track the previous room.
   It just prints an apologetic message asking for a direction instead."
  [game-state]
  (utils/tell game-state "Sorry, my memory is poor. Please give a direction."))

;;; ---------------------------------------------------------------------------
;;; TURN COMMAND
;;; ---------------------------------------------------------------------------
;;; ZIL: V-TURN in gverbs.zil

(defn v-turn
  "Turn an object (e.g., turn bolt with wrench).

   ZIL: V-TURN in gverbs.zil (line 1521)
     <ROUTINE V-TURN ()
       <TELL \"This has no effect.\" CR>>

   First tries the object's action handler (like V-MOVE does).
   If not handled, outputs 'This has no effect.'"
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        obj (gs/get-thing game-state prso)
        action-fn (:action obj)]
    ;; First try the object's action handler
    (if-let [result (when action-fn (action-fn game-state))]
      ;; Object handled the verb
      result
      ;; Object didn't handle it - default behavior
      (utils/tell game-state "This has no effect."))))
