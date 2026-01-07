(ns clork.verbs-misc
  "Miscellaneous verb handlers - simple actions, Easter eggs, and communication.

   ZIL source: gverbs.zil"
  (:require [clork.utils :as utils]
            [clork.game-state :as gs]
            [clork.parser.state :as parser-state]
            [clork.random :as random]
            [clork.verbs-movement :as verbs-movement]))

;;; ---------------------------------------------------------------------------
;;; RANDOM MESSAGE TABLES
;;; ---------------------------------------------------------------------------

(def ho-hum-messages
  "Generic 'nothing happens' messages for HACK-HACK.
   ZIL: HO-HUM table in gverbs.zil"
  [" doesn't seem to work."
   " isn't notably helpful."
   " has no effect."])

(def hello-messages
  "Greetings when saying hello to no one.
   ZIL: HELLOS table in gverbs.zil"
  ["Hello."
   "Good day."
   "Nice weather we've been having lately."
   "Goodbye."])

(def wheee-messages
  "Messages for skipping/jumping actions.
   ZIL: WHEEEEE table in gverbs.zil"
  ["Very good. Now you can go to the second grade."
   "Are you enjoying yourself?"
   "Wheeeeeeeeee!!!!!"
   "Do you expect me to applaud?"])

(def jump-death-messages
  "Death messages for fatal jumps.
   ZIL: JUMPLOSS table in gverbs.zil"
  ["You should have looked before you leaped."
   "In the movies, your life would be passing before your eyes."
   "Geronimo..."])

;;; ---------------------------------------------------------------------------
;;; HELPER FUNCTIONS
;;; ---------------------------------------------------------------------------

(defn hack-hack
  "Generic action handler that prints '[prefix][object][random ho-hum message]'.
   Used by WAVE, RUB, KICK, PUSH etc.
   ZIL: HACK-HACK in gverbs.zil lines 2040-2045"
  [game-state prefix]
  (let [prso (parser-state/get-prso game-state)
        obj-name (if prso (gs/thing-name game-state prso) "it")
        message (random/rand-nth* ho-hum-messages)]
    (-> game-state
        (utils/tell (str prefix obj-name message))
        (utils/crlf))))

(defn v-skip
  "Random 'nothing happened' message for jumping.
   ZIL: V-SKIP in gverbs.zil"
  [game-state]
  (-> game-state
      (utils/tell (random/rand-nth* wheee-messages))
      (utils/crlf)))

;;; ---------------------------------------------------------------------------
;;; SIMPLE/MESSAGE VERBS
;;; ---------------------------------------------------------------------------

(defn v-jump
  "Handle JUMP/LEAP verb.
   ZIL: V-LEAP in gverbs.zil lines 829-858"
  [game-state]
  (let [prso (:prso game-state)
        here (:here game-state)]
    (cond
      ;; Jump over something
      prso
      (let [obj (gs/get-thing game-state prso)]
        (if (contains? (:flags obj #{}) :actor)
          (-> game-state
              (utils/tell (str "The " (gs/thing-name game-state prso) " is too big to jump over."))
              (utils/crlf))
          (v-skip game-state)))

      ;; Special case: jumping in up-a-tree goes down safely
      (= here :up-a-tree)
      (-> game-state
          (utils/tell "In a feat of unaccustomed daring, you manage to land on your feet without killing yourself.")
          (utils/crlf)
          (utils/crlf)
          (assoc :here :forest-path))

      ;; Default: skip message
      :else
      (v-skip game-state))))

(defn v-swim
  "Handle SWIM verb.
   ZIL: V-SWIM in gverbs.zil lines 1340-1361"
  [game-state]
  (let [here (:here game-state)
        room (gs/get-thing game-state here)
        has-water? (contains? (:globals room #{}) :global-water)]
    (if has-water?
      (-> game-state
          (utils/tell "Swimming isn't usually allowed in the dungeon.")
          (utils/crlf))
      (-> game-state
          (utils/tell "Go jump in a lake!")
          (utils/crlf)))))

(defn v-listen
  "Handle LISTEN verb.
   ZIL: V-LISTEN in gverbs.zil line 868-869"
  [game-state]
  (let [prso (:prso game-state)]
    (if prso
      (-> game-state
          (utils/tell (str "The " (gs/thing-name game-state prso) " makes no sound."))
          (utils/crlf))
      (-> game-state
          (utils/tell "You hear nothing unexpected.")
          (utils/crlf)))))

(defn v-smell
  "Handle SMELL/SNIFF verb.
   ZIL: V-SMELL in gverbs.zil line 1294-1295"
  [game-state]
  (let [prso (:prso game-state)]
    (if prso
      (-> game-state
          (utils/tell (str "It smells like a " (gs/thing-name game-state prso) "."))
          (utils/crlf))
      (-> game-state
          (utils/tell "You smell nothing unexpected.")
          (utils/crlf)))))

(defn v-kick
  "Handle KICK verb.
   ZIL: V-KICK in gverbs.zil line 776"
  [game-state]
  (hack-hack game-state "Kicking the "))

(defn v-kiss
  "Handle KISS verb.
   ZIL: V-KISS in gverbs.zil lines 778-779"
  [game-state]
  (-> game-state
      (utils/tell "I'd sooner kiss a pig.")
      (utils/crlf)))

(defn v-knock
  "Handle KNOCK verb.
   ZIL: V-KNOCK in gverbs.zil lines 781-785"
  [game-state]
  (let [prso (:prso game-state)
        obj (when prso (gs/get-thing game-state prso))
        is-door? (contains? (:flags obj #{}) :door)]
    (if is-door?
      (-> game-state
          (utils/tell "Nobody's home.")
          (utils/crlf))
      (-> game-state
          (utils/tell (str "Why knock on a " (gs/thing-name game-state prso) "?"))
          (utils/crlf)))))

(defn v-pray
  "Handle PRAY verb.
   ZIL: V-PRAY in gverbs.zil lines 1062-1070
   Special: In South Temple, teleports to forest (no message, just teleport)."
  [game-state]
  (if (= (:here game-state) :south-temple)
    ;; ZIL: Just <GOTO ,FOREST-1> with no message
    (verbs-movement/goto game-state :forest-1)
    (-> game-state
        (utils/tell "If you pray enough, your prayers may be answered.")
        (utils/crlf))))

(defn v-stand
  "Handle STAND verb.
   ZIL: V-STAND in gverbs.zil lines 1321-1326"
  [game-state]
  ;; TODO: If in vehicle, disembark
  (-> game-state
      (utils/tell "You are already standing, I think.")
      (utils/crlf)))

(defn v-find
  "Handle FIND/WHERE verb.
   ZIL: V-FIND in gverbs.zil lines 693-715"
  [game-state]
  (let [prso (:prso game-state)
        loc (when prso (gs/get-thing-loc-id game-state prso))
        here (:here game-state)
        winner (:winner game-state)]
    (cond
      ;; Special objects
      (#{:hands :lungs} prso)
      (-> game-state
          (utils/tell "Within six feet of your head, assuming you haven't left that somewhere.")
          (utils/crlf))

      (= prso :me)
      (-> game-state
          (utils/tell "You're around here somewhere...")
          (utils/crlf))

      ;; Player has it
      (= loc winner)
      (-> game-state
          (utils/tell "You have it.")
          (utils/crlf))

      ;; It's in the room
      (or (= loc here)
          (contains? (:globals (gs/get-thing game-state here) #{}) prso))
      (-> game-state
          (utils/tell "It's right here.")
          (utils/crlf))

      ;; In a container
      (and loc (gs/get-thing game-state loc))
      (let [container (gs/get-thing game-state loc)]
        (cond
          (contains? (:flags container #{}) :actor)
          (-> game-state
              (utils/tell (str "The " (gs/thing-name game-state loc) " has it."))
              (utils/crlf))

          (contains? (:flags container #{}) :surface)
          (-> game-state
              (utils/tell (str "It's on the " (gs/thing-name game-state loc) "."))
              (utils/crlf))

          (contains? (:flags container #{}) :cont)
          (-> game-state
              (utils/tell (str "It's in the " (gs/thing-name game-state loc) "."))
              (utils/crlf))

          :else
          (-> game-state
              (utils/tell "Beats me.")
              (utils/crlf))))

      :else
      (-> game-state
          (utils/tell "Beats me.")
          (utils/crlf)))))

(defn v-count
  "Handle COUNT verb.
   ZIL: V-COUNT in gverbs.zil lines 381-385"
  [game-state]
  (let [prso (:prso game-state)]
    (if (= prso :blessings)
      (-> game-state
          (utils/tell "Well, for one, you are playing Zork...")
          (utils/crlf))
      (-> game-state
          (utils/tell "You have lost your mind.")
          (utils/crlf)))))

(defn v-ring
  "Handle RING verb (ring a bell).
   ZIL: V-RING in gverbs.zil lines 1178-1179
   First checks object action handler (e.g., brass bell at entrance to hades)."
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        obj (when prso (gs/get-thing game-state prso))
        action-fn (:action obj)
        here (:here game-state)]
    ;; First try the object's action handler
    (if-let [result (when action-fn (action-fn game-state))]
      result
      ;; If in Entrance to Hades with bell and exorcism not done, room handles it
      (if (and (= here :entrance-to-hades)
               (= prso :brass-bell)
               (not (:lld-flag game-state)))
        ;; Let room handle exorcism
        (let [room (gs/get-thing game-state :entrance-to-hades)
              room-action (:action room)]
          (if room-action
            (room-action game-state :ring-bell)
            (utils/tell game-state "You haven't implemented the exorcism yet.")))
        ;; Default behavior
        (-> game-state
            (utils/tell "How, exactly, can you ring that?")
            (utils/crlf))))))

;;; ---------------------------------------------------------------------------
;;; OBJECT MANIPULATION VERBS
;;; ---------------------------------------------------------------------------

(defn v-wave
  "Handle WAVE verb.
   ZIL: V-WAVE in gverbs.zil line 1610-1611
   First checks object action handler (e.g., sceptre at rainbow)."
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        obj (when prso (gs/get-thing game-state prso))
        action-fn (:action obj)]
    ;; First try the object's action handler (e.g., sceptre)
    (if-let [result (when action-fn (action-fn game-state))]
      result
      ;; Default behavior
      (hack-hack game-state "Waving the "))))

(defn v-rub
  "Handle RUB/TOUCH verb.
   ZIL: V-RUB in gverbs.zil line 1181
   First checks object action handler (e.g., mirror)."
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        obj (when prso (gs/get-thing game-state prso))
        action-fn (:action obj)]
    ;; First try the object's action handler (e.g., mirror)
    (if-let [result (when action-fn (action-fn game-state))]
      result
      ;; Default behavior
      (hack-hack game-state "Fiddling with the "))))

(defn v-raise
  "Handle RAISE/LIFT verb.
   ZIL: V-RAISE uses HACK-HACK
   First checks the object's action handler (e.g., for basket)."
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        obj (when prso (gs/get-thing game-state prso))
        action-fn (:action obj)]
    ;; First try the object's action handler
    (if-let [result (when action-fn (action-fn game-state))]
      result
      ;; Default behavior
      (hack-hack game-state "Lifting the "))))

(defn v-lower
  "Handle LOWER verb.
   ZIL: V-LOWER uses HACK-HACK
   First checks the object's action handler (e.g., for basket)."
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        obj (when prso (gs/get-thing game-state prso))
        action-fn (:action obj)]
    ;; First try the object's action handler
    (if-let [result (when action-fn (action-fn game-state))]
      result
      ;; Default behavior
      (hack-hack game-state "Lowering the "))))

(defn v-shake
  "Handle SHAKE verb.
   First checks the object's action handler (e.g., for bottle)."
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        obj (when prso (gs/get-thing game-state prso))
        action-fn (:action obj)]
    ;; First try the object's action handler
    (if-let [result (when action-fn (action-fn game-state))]
      result
      ;; Default behavior
      (hack-hack game-state "Shaking the "))))

(defn v-tie
  "Handle TIE verb.
   ZIL: V-TIE in gverbs.zil lines 1481-1485
   First checks the object's action handler (e.g., for rope)."
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        prsi (parser-state/get-prsi game-state)
        obj (when prso (gs/get-thing game-state prso))
        action-fn (:action obj)
        winner (:winner game-state)]
    ;; First try the object's action handler
    (if-let [result (when action-fn (action-fn game-state))]
      result
      ;; Default behavior
      (cond
        (nil? prso)
        (-> game-state
            (utils/tell "You need to specify what to tie.")
            (utils/crlf))
        (= prsi winner)
        (-> game-state
            (utils/tell "You can't tie anything to yourself.")
            (utils/crlf))
        :else
        (-> game-state
            (utils/tell (str "You can't tie the " (gs/thing-name game-state prso) " to that."))
            (utils/crlf))))))

(defn v-untie
  "Handle UNTIE verb.
   ZIL: V-UNTIE in gverbs.zil lines 1527-1528
   First checks the object's action handler (e.g., for rope)."
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        obj (when prso (gs/get-thing game-state prso))
        action-fn (:action obj)]
    ;; First try the object's action handler
    (if-let [result (when action-fn (action-fn game-state))]
      result
      ;; Default behavior
      (-> game-state
          (utils/tell "This cannot be tied, so it cannot be untied!")
          (utils/crlf)))))

(defn v-swing
  "Handle SWING verb.
   ZIL: V-SWING in gverbs.zil lines 1363-1367"
  [game-state]
  (let [prsi (:prsi game-state)]
    (if prsi
      ;; Swing at something = attack
      (-> game-state
          (assoc :verb :attack)
          (assoc :prso prsi)
          (assoc :prsi (:prso game-state)))
      (-> game-state
          (utils/tell "Whoosh!")
          (utils/crlf)))))

;;; ---------------------------------------------------------------------------
;;; COMMUNICATION VERBS
;;; ---------------------------------------------------------------------------

(defn v-hello
  "Handle HELLO/HI verb.
   ZIL: V-HELLO in gverbs.zil lines 740-750"
  [game-state]
  (let [prso (:prso game-state)]
    (if prso
      (let [obj (gs/get-thing game-state prso)
            is-actor? (contains? (:flags obj #{}) :actor)]
        (if is-actor?
          (-> game-state
              (utils/tell (str "The " (gs/thing-name game-state prso) " bows his head to you in greeting."))
              (utils/crlf))
          (-> game-state
              (utils/tell (str "It's a well known fact that only schizophrenics say \"Hello\" to a " (gs/thing-name game-state prso) "."))
              (utils/crlf))))
      (-> game-state
          (utils/tell (random/rand-nth* hello-messages))
          (utils/crlf)))))

(defn v-yell
  "Handle YELL/SCREAM verb.
   ZIL: V-YELL in gverbs.zil line 1632"
  [game-state]
  (-> game-state
      (utils/tell "Aaaarrrrgggghhhh!")
      (utils/crlf)))

(defn v-say
  "Handle SAY verb.
   ZIL: V-SAY in gverbs.zil lines 1183-1209"
  [game-state]
  ;; TODO: More complex say handling with actors
  (-> game-state
      (utils/tell "Talking to yourself is a sign of impending mental collapse.")
      (utils/crlf)))

;;; ---------------------------------------------------------------------------
;;; EASTER EGG / META VERBS
;;; ---------------------------------------------------------------------------

(defn v-curses
  "Handle CURSE/DAMN verb.
   ZIL: V-CURSES in gverbs.zil lines 390-398"
  [game-state]
  (let [prso (:prso game-state)]
    (if prso
      (let [obj (gs/get-thing game-state prso)
            is-actor? (contains? (:flags obj #{}) :actor)]
        (if is-actor?
          (-> game-state
              (utils/tell "Insults of this nature won't help you.")
              (utils/crlf))
          (-> game-state
              (utils/tell "What a loony!")
              (utils/crlf))))
      (-> game-state
          (utils/tell "Such language in a high-class establishment like this!")
          (utils/crlf)))))

(defn v-win
  "Handle WIN verb.
   ZIL: V-WIN in gverbs.zil lines 1620-1621"
  [game-state]
  (-> game-state
      (utils/tell "Naturally!")
      (utils/crlf)))

(defn v-zork
  "Handle ZORK verb.
   ZIL: V-ZORK in gverbs.zil line 1634"
  [game-state]
  (-> game-state
      (utils/tell "At your service!")
      (utils/crlf)))

(defn v-frobozz
  "Handle FROBOZZ verb.
   ZIL: V-FROBOZZ in gverbs.zil lines 720-722"
  [game-state]
  (-> game-state
      (utils/tell "The FROBOZZ Corporation created, owns, and operates this dungeon.")
      (utils/crlf)))

(defn v-xyzzy
  "Handle XYZZY/PLUGH verb (Adventure easter egg).
   ZIL: V-ADVENT"
  [game-state]
  (-> game-state
      (utils/tell "A hollow voice says \"Fool.\"")
      (utils/crlf)))

(defn v-treasure
  "Handle TREASURE/TEMPLE verb (hint).
   ZIL: V-TREASURE in gverbs.zil lines 1490-1502
   Special: In North Temple, teleports to Treasure Room and vice versa."
  [game-state]
  (let [here (:here game-state)]
    (cond
      (= here :north-temple)
      (-> game-state
          (assoc :here :treasure-room))

      (= here :treasure-room)
      (-> game-state
          (assoc :here :north-temple))

      :else
      (-> game-state
          (utils/tell "Nothing happens.")
          (utils/crlf)))))

;;; ---------------------------------------------------------------------------
;;; TOOL VERBS (STUBS - to be expanded with objects)
;;; ---------------------------------------------------------------------------

(defn v-cut
  "Handle CUT/SLICE verb.
   ZIL: V-CUT in gverbs.zil lines 400-416"
  [game-state]
  (let [prsi (:prsi game-state)]
    (if (and prsi
             (let [tool (gs/get-thing game-state prsi)]
               (contains? (:flags tool #{}) :weapon)))
      (-> game-state
          (utils/tell (str "Strange concept, cutting the " (gs/thing-name game-state (:prso game-state)) "...."))
          (utils/crlf))
      (-> game-state
          (utils/tell (str "The \"cutting edge\" of a " (gs/thing-name game-state prsi) " is hardly adequate."))
          (utils/crlf)))))

(defn v-dig
  "Handle DIG verb.
   Note: Requires shovel implementation."
  [game-state]
  (-> game-state
      (utils/tell "Digging without a shovel is quite difficult.")
      (utils/crlf)))

(defn v-burn
  "Handle BURN/IGNITE verb.
   Note: Requires fire source implementation."
  [game-state]
  (-> game-state
      (utils/tell "You don't have anything to light it with.")
      (utils/crlf)))

(defn v-fill
  "Handle FILL verb."
  [game-state]
  (-> game-state
      (utils/tell "There's nothing to fill it with.")
      (utils/crlf)))

(defn v-pour
  "Handle POUR verb."
  [game-state]
  (-> game-state
      (utils/tell "You can't pour that.")
      (utils/crlf)))

(defn v-inflate
  "Handle INFLATE verb.
   ZIL: V-INFLATE in gverbs.zil lines 773-774
   First checks the object's action handler (for inflatable boat)."
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        obj (when prso (gs/get-thing game-state prso))
        action-fn (:action obj)]
    ;; First try the object's action handler
    (if-let [result (when action-fn (action-fn game-state))]
      result
      ;; Default behavior
      (-> game-state
          (utils/tell "How can you inflate that?")
          (utils/crlf)))))

(defn v-deflate
  "Handle DEFLATE verb.
   ZIL: V-DEFLATE in gverbs.zil lines 418-419
   First checks the object's action handler (for inflated boat)."
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        obj (when prso (gs/get-thing game-state prso))
        action-fn (:action obj)]
    ;; First try the object's action handler
    (if-let [result (when action-fn (action-fn game-state))]
      result
      ;; Default behavior
      (-> game-state
          (utils/tell "Come on, now!")
          (utils/crlf)))))

(defn v-cross
  "Handle CROSS verb.
   ZIL: V-CROSS in gverbs.zil lines 387-388
   First checks the object's action handler (e.g., for rainbow)."
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        obj (when prso (gs/get-thing game-state prso))
        action-fn (:action obj)]
    ;; First try the object's action handler
    (if-let [result (when action-fn (action-fn game-state))]
      result
      ;; Default behavior
      (-> game-state
          (utils/tell "You can't cross that!")
          (utils/crlf)))))

(defn v-launch
  "Handle LAUNCH verb.
   ZIL: V-LAUNCH in gverbs.zil lines 820-824

   If no object specified, checks if player is in a vehicle and uses that.
   The boat's action handler (RBOAT-FUNCTION) handles the actual launching."
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        ;; If no object, check if player is in a vehicle
        player-loc (gs/get-thing-loc-id game-state :adventurer)
        vehicle (when (and (nil? prso) (keyword? player-loc))
                  (let [loc-obj (gs/get-thing game-state player-loc)]
                    (when (contains? (or (:flags loc-obj) #{}) :vehicle)
                      player-loc)))
        ;; Use explicit object or inferred vehicle
        target (or prso vehicle)
        obj (when target (gs/get-thing game-state target))
        action-fn (:action obj)]
    (if target
      ;; Set PRSO if we inferred the vehicle
      (let [gs (if (nil? prso)
                 (assoc-in game-state [:parser :prso] target)
                 game-state)]
        ;; Try the object's action handler
        (if-let [result (when action-fn (action-fn gs))]
          result
          ;; Default - can't launch
          (-> gs
              (utils/tell "You can't launch that!")
              (utils/crlf))))
      ;; No object and not in vehicle
      (-> game-state
          (utils/tell "You're not in anything to launch!")
          (utils/crlf)))))

(defn v-board
  "Handle BOARD verb."
  [game-state]
  (-> game-state
      (utils/tell "You can't board that.")
      (utils/crlf)))

(defn v-disembark
  "Handle DISEMBARK/EXIT verb."
  [game-state]
  (-> game-state
      (utils/tell "You're not in anything.")
      (utils/crlf)))

(defn v-wear
  "Handle WEAR verb.
   ZIL: V-WEAR in gverbs.zil lines 1613-1618"
  [game-state]
  (let [prso (:prso game-state)
        obj (when prso (gs/get-thing game-state prso))]
    (if (and obj (contains? (:flags obj #{}) :wear))
      ;; TODO: Actually wear it (perform take)
      (-> game-state
          (utils/tell "Ok, you're now wearing it.")
          (utils/crlf))
      (-> game-state
          (utils/tell (str "You can't wear the " (gs/thing-name game-state prso) "."))
          (utils/crlf)))))

;;; ---------------------------------------------------------------------------
;;; NPC INTERACTION VERBS
;;; ---------------------------------------------------------------------------

(defn v-alarm
  "Handle WAKE/ALARM verb.
   ZIL: V-ALARM in gverbs.zil lines 172-184"
  [game-state]
  (let [prso (:prso game-state)
        obj (when prso (gs/get-thing game-state prso))]
    (cond
      ;; No object specified
      (nil? prso)
      (-> game-state
          (utils/tell "Wake what?")
          (utils/crlf))

      ;; It's an actor
      (and obj (contains? (:flags obj #{}) :actor))
      (-> game-state
          (utils/tell "He's wide awake, or haven't you noticed...")
          (utils/crlf))

      ;; Not an actor
      :else
      (-> game-state
          (utils/tell (str "The " (gs/thing-name game-state prso) " isn't sleeping."))
          (utils/crlf)))))

(defn v-tell
  "Handle TELL/TALK TO verb.
   ZIL: V-TELL in gverbs.zil lines 1405-1418"
  [game-state]
  (let [prso (:prso game-state)
        obj (when prso (gs/get-thing game-state prso))]
    (if (and obj (contains? (:flags obj #{}) :actor))
      (-> game-state
          (utils/tell (str "The " (gs/thing-name game-state prso)
                          " pauses for a moment, perhaps thinking that you should reread the manual."))
          (utils/crlf))
      (-> game-state
          (utils/tell (str "You can't talk to the " (gs/thing-name game-state prso) "!"))
          (utils/crlf)))))

(defn v-answer
  "Handle ANSWER verb.
   ZIL: V-ANSWER in gverbs.zil lines 186-190"
  [game-state]
  (-> game-state
      (utils/tell "Nobody seems to be awaiting your answer.")
      (utils/crlf)))

(defn v-reply
  "Handle REPLY verb (answer to specific NPC).
   ZIL: V-REPLY in gverbs.zil lines 1172-1176"
  [game-state]
  (let [prso (:prso game-state)]
    (-> game-state
        (utils/tell (str "It is hardly likely that the " (gs/thing-name game-state prso) " is interested."))
        (utils/crlf))))

(defn v-command
  "Handle COMMAND verb.
   ZIL: V-COMMAND in gverbs.zil lines 375-379"
  [game-state]
  (let [prso (:prso game-state)
        obj (when prso (gs/get-thing game-state prso))]
    (if (and obj (contains? (:flags obj #{}) :actor))
      (-> game-state
          (utils/tell (str "The " (gs/thing-name game-state prso) " pays no attention."))
          (utils/crlf))
      (-> game-state
          (utils/tell "You cannot talk to that!")
          (utils/crlf)))))

(defn v-follow
  "Handle FOLLOW verb.
   ZIL: V-FOLLOW in gverbs.zil lines 717-718"
  [game-state]
  (-> game-state
      (utils/tell "You're nuts!")
      (utils/crlf)))

(defn v-send
  "Handle SEND FOR verb.
   ZIL: V-SEND in gverbs.zil lines 1216-1220"
  [game-state]
  (let [prso (:prso game-state)
        obj (when prso (gs/get-thing game-state prso))]
    (if (and obj (contains? (:flags obj #{}) :actor))
      (-> game-state
          (utils/tell (str "Why would you send for the " (gs/thing-name game-state prso) "?"))
          (utils/crlf))
      (-> game-state
          (utils/tell "That doesn't make sends.")
          (utils/crlf)))))

;;; ---------------------------------------------------------------------------
;;; COMBAT/VIOLENCE VERBS
;;; ---------------------------------------------------------------------------

(defn v-blast
  "Handle BLAST/BLOW UP verb.
   ZIL: V-BLAST in gverbs.zil line 214-215"
  [game-state]
  (-> game-state
      (utils/tell "You can't blast anything by using words.")
      (utils/crlf)))

(defn v-mung
  "Handle MUNG/DESTROY verb.
   ZIL: V-MUNG in gverbs.zil lines 954-959

   First checks the object's action handler (e.g., for egg destruction).
   Then falls back to default behavior."
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        obj (when prso (gs/get-thing game-state prso))
        action-fn (:action obj)]
    ;; First try the object's action handler (e.g., egg)
    (if-let [result (when action-fn (action-fn game-state))]
      result
      ;; Object didn't handle it - default behavior
      (if (and obj (contains? (:flags obj #{}) :actor))
        ;; Redirect to attack
        (-> game-state
            (assoc :verb :attack))
        (-> game-state
            (utils/tell "Nice try.")
            (utils/crlf))))))

(defn v-strike
  "Handle STRIKE verb.
   ZIL: V-STRIKE in gverbs.zil lines 1331-1338"
  [game-state]
  (let [prso (:prso game-state)
        obj (when prso (gs/get-thing game-state prso))]
    (if (and obj (contains? (:flags obj #{}) :actor))
      (-> game-state
          (utils/tell (str "Since you aren't versed in hand-to-hand combat, you'd better attack the "
                          (gs/thing-name game-state prso) " with a weapon."))
          (utils/crlf))
      ;; Non-actor: try to light it (strike a match)
      (-> game-state
          (assoc :verb :lamp-on)))))

(defn v-throw
  "Handle THROW verb.
   ZIL: V-THROW in gverbs.zil lines 1461-1476

   First checks the object's action handler (e.g., for throwing egg).
   Then falls back to default behavior."
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        prsi (parser-state/get-prsi game-state)
        obj (when prso (gs/get-thing game-state prso))
        action-fn (:action obj)]
    ;; First try the object's action handler (e.g., egg)
    (if-let [result (when action-fn (action-fn game-state))]
      result
      ;; Object didn't handle it - default behavior
      (cond
        ;; Throw at self
        (= prsi :me)
        (-> game-state
            (utils/tell (str "A terrific throw! The " (gs/thing-name game-state prso)
                            " hits you squarely in the head. Normally, this wouldn't do much damage, but by incredible mischance, you fall over backwards trying to duck, and break your neck, justice being swift and merciful in the Great Underground Empire."))
            (utils/crlf)
            (assoc :dead true))

        ;; Throw at actor
        (and prsi
             (let [target (gs/get-thing game-state prsi)]
               (contains? (:flags target #{}) :actor)))
        (-> game-state
            (utils/tell (str "The " (gs/thing-name game-state prsi)
                            " ducks as the " (gs/thing-name game-state prso)
                            " flies by and crashes to the ground."))
            (utils/crlf))

        ;; Default
        :else
        (-> game-state
            (utils/tell "Thrown.")
            (utils/crlf))))))

(defn v-throw-off
  "Handle THROW OFF/OVER verb.
   ZIL: V-THROW-OFF in gverbs.zil lines 1478-1479"
  [game-state]
  (-> game-state
      (utils/tell "You can't throw anything off of that!")
      (utils/crlf)))

(defn v-overboard
  "Handle throwing something overboard from a vehicle.
   ZIL: V-OVERBOARD in gverbs.zil lines 1012-1025"
  [game-state]
  (let [winner-loc (gs/get-thing-loc-id game-state (:winner game-state))
        vehicle (gs/get-thing game-state winner-loc)]
    (if (and vehicle (contains? (:flags vehicle #{}) :vehicle))
      (-> game-state
          (utils/tell (str "Ahoy -- " (gs/thing-name game-state (:prso game-state)) " overboard!"))
          (utils/crlf))
      (-> game-state
          (utils/tell "You're not in anything!")
          (utils/crlf)))))

(defn v-tie-up
  "Handle TIE UP verb.
   ZIL: V-TIE-UP in gverbs.zil lines 1487-1488"
  [game-state]
  (-> game-state
      (utils/tell "You could certainly never tie it with that!")
      (utils/crlf)))

;;; ---------------------------------------------------------------------------
;;; MAGIC VERBS (stubs for Zork II/III)
;;; ---------------------------------------------------------------------------

(defn v-incant
  "Handle INCANT verb (magic spell).
   ZIL: V-INCANT - Zork II/III magic system stub"
  [game-state]
  (-> game-state
      (utils/tell "Nothing happens.")
      (utils/crlf)))

(defn v-enchant
  "Handle ENCHANT verb.
   ZIL: V-ENCHANT - Zork II/III magic system stub"
  [game-state]
  (-> game-state
      (utils/tell "Nothing happens.")
      (utils/crlf)))

(defn v-disenchant
  "Handle DISENCHANT verb.
   ZIL: V-DISENCHANT - Zork II/III magic system stub"
  [game-state]
  (-> game-state
      (utils/tell "Nothing happens.")
      (utils/crlf)))

(defn v-exorcise
  "Handle EXORCISE verb.
   ZIL: V-EXORCISE in gverbs.zil lines 659-660"
  [game-state]
  (-> game-state
      (utils/tell "What a bizarre concept!")
      (utils/crlf)))

;;; ---------------------------------------------------------------------------
;;; OBJECT ACTION VERBS
;;; ---------------------------------------------------------------------------

(defn v-brush
  "Handle BRUSH verb.
   ZIL: V-BRUSH in gverbs.zil lines 249-250"
  [game-state]
  (-> game-state
      (utils/tell "If you wish, but heaven only knows why.")
      (utils/crlf)))

(defn v-squeeze
  "Handle SQUEEZE verb.
   ZIL: V-SQUEEZE in gverbs.zil lines 1303-1308"
  [game-state]
  (let [prso (:prso game-state)
        obj (when prso (gs/get-thing game-state prso))]
    (if (and obj (contains? (:flags obj #{}) :actor))
      (-> game-state
          (utils/tell (str "The " (gs/thing-name game-state prso) " does not understand this."))
          (utils/crlf))
      (-> game-state
          (utils/tell "How singularly useless.")
          (utils/crlf)))))

(defn v-spin
  "Handle SPIN verb.
   ZIL: V-SPIN in gverbs.zil lines 1297-1298"
  [game-state]
  (-> game-state
      (utils/tell "You can't spin that!")
      (utils/crlf)))

(defn v-wind
  "Handle WIND/WIND UP verb.
   ZIL: V-WIND in gverbs.zil lines 1623-1624"
  [game-state]
  (-> game-state
      (utils/tell (str "You cannot wind up a " (gs/thing-name game-state (:prso game-state)) "."))
      (utils/crlf)))

(defn v-pick
  "Handle PICK verb (pick lock).
   ZIL: V-PICK in gverbs.zil line 1027"
  [game-state]
  (-> game-state
      (utils/tell "You can't pick that.")
      (utils/crlf)))

(defn v-oil
  "Handle OIL/LUBRICATE verb.
   ZIL: V-OIL in gverbs.zil lines 979-980"
  [game-state]
  (-> game-state
      (utils/tell "You probably put spinach in your gas tank, too.")
      (utils/crlf)))

(defn v-melt
  "Handle MELT verb.
   ZIL: V-MELT in gverbs.zil lines 923-924"
  [game-state]
  (-> game-state
      (utils/tell (str "It's not clear that a " (gs/thing-name game-state (:prso game-state)) " can be melted."))
      (utils/crlf)))

(defn v-spray
  "Handle SPRAY verb.
   ZIL: V-SPRAY in gverbs.zil line 1300-1301 - redirects to V-SQUEEZE"
  [game-state]
  (v-squeeze game-state))

;;; ---------------------------------------------------------------------------
;;; CONTAINER/POSITION VERBS
;;; ---------------------------------------------------------------------------

(defn v-drink-from
  "Handle DRINK FROM verb.
   ZIL: V-DRINK-FROM - redirects to V-DRINK"
  [game-state]
  ;; Just redirect to drink
  (-> game-state
      (assoc :verb :drink)))

(defn v-push-to
  "Handle PUSH TO verb.
   ZIL: V-PUSH-TO in gverbs.zil lines 1088-1089"
  [game-state]
  (-> game-state
      (utils/tell "You can't push things to that.")
      (utils/crlf)))

(defn v-put-under
  "Handle PUT UNDER verb.
   ZIL: V-PUT-UNDER in gverbs.zil lines 1144-1145"
  [game-state]
  (-> game-state
      (utils/tell "You can't do that.")
      (utils/crlf)))

(defn v-put-behind
  "Handle PUT BEHIND verb.
   ZIL: V-PUT-BEHIND in gverbs.zil lines 1132-1133"
  [game-state]
  (-> game-state
      (utils/tell "That hiding place is too obvious.")
      (utils/crlf)))

(defn v-search
  "Handle SEARCH verb.
   ZIL: V-SEARCH in gverbs.zil lines 1213-1214"
  [game-state]
  (-> game-state
      (utils/tell "You find nothing unusual.")
      (utils/crlf)))

(defn v-pour-on
  "Handle POUR ON verb.
   ZIL: V-POUR-ON in gverbs.zil lines 1040-1070"
  [game-state]
  (let [prso (:prso game-state)
        prsi (:prsi game-state)]
    ;; TODO: Check if pouring water on fire
    (-> game-state
        (utils/tell "You can't pour that on anything.")
        (utils/crlf))))

(defn v-plug
  "Handle PLUG/PATCH verb.
   ZIL: V-PLUG in gverbs.zil lines 1037-1038"
  [game-state]
  (-> game-state
      (utils/tell "This has no effect.")
      (utils/crlf)))

(defn v-pump
  "Handle PUMP verb.
   ZIL: V-PUMP in gverbs.zil lines 1072-1084"
  [game-state]
  (let [prsi (:prsi game-state)]
    (cond
      ;; Pumping with something other than pump
      (and prsi (not= prsi :pump))
      (-> game-state
          (utils/tell (str "Pump it up with a " (gs/thing-name game-state prsi) "?"))
          (utils/crlf))

      :else
      (-> game-state
          (utils/tell "It's really not clear how.")
          (utils/crlf)))))

;;; ---------------------------------------------------------------------------
;;; MISCELLANEOUS VERBS
;;; ---------------------------------------------------------------------------

(defn v-breathe
  "Handle BREATHE/BLOW IN verb.
   ZIL: V-BREATHE in gverbs.zil line 246-247 - redirects to V-INFLATE with lungs"
  [game-state]
  ;; Redirect to inflate using lungs
  (-> game-state
      (assoc :verb :inflate)
      (assoc :prsi :lungs)))

(defn v-chomp
  "Handle CHOMP/BITE verb.
   ZIL: V-CHOMP in gverbs.zil lines 292-293"
  [game-state]
  (-> game-state
      (utils/tell "Preposterous!")
      (utils/crlf)))

(defn v-lean-on
  "Handle LEAN ON verb.
   ZIL: V-LEAN-ON in gverbs.zil lines 826-827"
  [game-state]
  (-> game-state
      (utils/tell "Getting tired?")
      (utils/crlf)))

(defn v-make
  "Handle MAKE verb.
   ZIL: V-MAKE in gverbs.zil lines 920-921"
  [game-state]
  (-> game-state
      (utils/tell "You can't do that.")
      (utils/crlf)))

(defn v-play
  "Handle PLAY verb.
   ZIL: V-PLAY in gverbs.zil lines 1029-1035"
  [game-state]
  (let [prso (:prso game-state)
        obj (when prso (gs/get-thing game-state prso))]
    (if (and obj (contains? (:flags obj #{}) :actor))
      (-> game-state
          (utils/tell (str "You become so engrossed in the role of the " (gs/thing-name game-state prso)
                          " that you kill yourself, just as he might have done!"))
          (utils/crlf)
          (assoc :dead true))
      (-> game-state
          (utils/tell "That's silly!")
          (utils/crlf)))))

(defn v-stay
  "Handle STAY verb.
   ZIL: V-STAY in gverbs.zil lines 1328-1329"
  [game-state]
  (-> game-state
      (utils/tell "You will be lost without me!")
      (utils/crlf)))

(defn v-wish
  "Handle WISH verb.
   ZIL: V-WISH in gverbs.zil lines 1626-1630"
  [game-state]
  (-> game-state
      (utils/tell "With luck, your wish will come true.")
      (utils/crlf)))

(defn v-hatch
  "Handle HATCH verb.
   ZIL: V-HATCH in gverbs.zil lines 735-736"
  [game-state]
  (-> game-state
      (utils/tell "Bizarre!")
      (utils/crlf)))

(defn v-mumble
  "Handle MUMBLE verb.
   ZIL: V-MUMBLE in gverbs.zil lines 936-937"
  [game-state]
  (-> game-state
      (utils/tell "You'll have to speak up if you expect me to hear you!")
      (utils/crlf)))

(defn v-repent
  "Handle REPENT verb.
   ZIL: V-REPENT in gverbs.zil lines 1169-1170"
  [game-state]
  (-> game-state
      (utils/tell "It could very well be too late!")
      (utils/crlf)))

;;; ---------------------------------------------------------------------------
;;; META/DEBUG VERBS
;;; ---------------------------------------------------------------------------

(defn v-bug
  "Handle BUG verb.
   ZIL: V-BUG in gverbs.zil lines 252-254"
  [game-state]
  (-> game-state
      (utils/tell "Bug? Not in a flawless program like this! (Cough, cough).")
      (utils/crlf)))

(defn v-verify
  "Handle VERIFY verb (disk verification - stub).
   ZIL: V-VERIFY in gverbs.zil lines 139-144"
  [game-state]
  (-> game-state
      (utils/tell "Verifying disk...")
      (utils/crlf)
      (utils/tell "The disk is correct.")
      (utils/crlf)))

