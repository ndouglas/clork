(ns clork.verbs-misc
  "Miscellaneous verb handlers - simple actions, Easter eggs, and communication.

   ZIL source: gverbs.zil"
  (:require [clork.utils :as utils]
            [clork.game-state :as gs]
            [clork.random :as random]))

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
  (let [prso (:prso game-state)
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
   Special: In South Temple, teleports to forest."
  [game-state]
  (if (= (:here game-state) :south-temple)
    (-> game-state
        (utils/tell "Your prayers have been answered.")
        (utils/crlf)
        (assoc :here :forest-1))
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
   ZIL: V-RING in gverbs.zil lines 1178-1179"
  [game-state]
  ;; TODO: Special handling for the brass bell
  (-> game-state
      (utils/tell "How, exactly, can you ring that?")
      (utils/crlf)))

;;; ---------------------------------------------------------------------------
;;; OBJECT MANIPULATION VERBS
;;; ---------------------------------------------------------------------------

(defn v-wave
  "Handle WAVE verb.
   ZIL: V-WAVE in gverbs.zil line 1610-1611
   Note: Sceptre waving at rainbow is handled by sceptre object action."
  [game-state]
  ;; The sceptre special case will be handled by object actions
  (hack-hack game-state "Waving the "))

(defn v-rub
  "Handle RUB/TOUCH verb.
   ZIL: V-RUB in gverbs.zil line 1181
   Note: Mirror rubbing is handled by mirror object action."
  [game-state]
  ;; Mirror special case will be handled by object actions
  (hack-hack game-state "Fiddling with the "))

(defn v-raise
  "Handle RAISE/LIFT verb.
   ZIL: V-RAISE uses HACK-HACK"
  [game-state]
  (hack-hack game-state "Lifting the "))

(defn v-lower
  "Handle LOWER verb.
   ZIL: V-LOWER uses HACK-HACK"
  [game-state]
  (hack-hack game-state "Lowering the "))

(defn v-shake
  "Handle SHAKE verb."
  [game-state]
  (hack-hack game-state "Shaking the "))

(defn v-tie
  "Handle TIE verb.
   ZIL: V-TIE in gverbs.zil lines 1481-1485
   Note: Rope tying to railing is handled by rope object action."
  [game-state]
  (let [prsi (:prsi game-state)
        winner (:winner game-state)]
    (if (= prsi winner)
      (-> game-state
          (utils/tell "You can't tie anything to yourself.")
          (utils/crlf))
      (-> game-state
          (utils/tell (str "You can't tie the " (gs/thing-name game-state (:prso game-state)) " to that."))
          (utils/crlf)))))

(defn v-untie
  "Handle UNTIE verb.
   ZIL: V-UNTIE in gverbs.zil lines 1527-1528"
  [game-state]
  ;; Rope untying will be handled by rope object action
  (-> game-state
      (utils/tell "This cannot be tied, so it cannot be untied!")
      (utils/crlf)))

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
   ZIL: V-INFLATE in gverbs.zil lines 773-774"
  [game-state]
  (-> game-state
      (utils/tell "How can you inflate that?")
      (utils/crlf)))

(defn v-deflate
  "Handle DEFLATE verb.
   ZIL: V-DEFLATE in gverbs.zil lines 418-419"
  [game-state]
  (-> game-state
      (utils/tell "Come on, now!")
      (utils/crlf)))

(defn v-cross
  "Handle CROSS verb.
   ZIL: V-CROSS in gverbs.zil lines 387-388"
  [game-state]
  (-> game-state
      (utils/tell "You can't cross that!")
      (utils/crlf)))

(defn v-launch
  "Handle LAUNCH verb.
   ZIL: V-LAUNCH in gverbs.zil lines 820-824"
  [game-state]
  (-> game-state
      (utils/tell "You can't launch that by saying \"launch\"!")
      (utils/crlf)))

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
