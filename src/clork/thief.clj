(ns clork.thief
  "Thief NPC implementation - ZIL ROBBER-FUNCTION equivalent.

   The Thief is a wandering NPC who steals valuables from rooms and the player,
   depositing them in his Treasure Room hideaway. Unlike the Troll, the Thief
   moves around the dungeon via the I-THIEF daemon.

   ZIL Reference:
   - ROBBER-FUNCTION in 1actions.zil (lines 1960-2097)
   - I-THIEF daemon in 1actions.zil (lines 3903-3993)
   - THIEF-VS-ADVENTURER in 1actions.zil (lines 1777-1810)"
  (:require [clork.utils :as utils]
            [clork.game-state :as gs]
            [clork.parser.state :as parser-state]
            [clork.random :as random]
            [clork.daemon :as daemon]
            [clork.debug.trace :as trace]))

;;; ---------------------------------------------------------------------------
;;; THIEF DESCRIPTIONS
;;; ---------------------------------------------------------------------------

(def robber-c-desc
  "Conscious thief description."
  "There is a suspicious-looking individual, holding a large bag, leaning against one wall. He is armed with a deadly stiletto.")

(def robber-u-desc
  "Unconscious thief description."
  "There is a suspicious-looking individual lying unconscious on the ground.")

;;; ---------------------------------------------------------------------------
;;; HELPER FUNCTIONS
;;; ---------------------------------------------------------------------------

(defn recover-stiletto
  "Ensure the thief has his stiletto.
   ZIL: RECOVER-STILETTO routine (1actions.zil lines 3962-3965)"
  [game-state]
  (let [thief-loc (gs/get-thing-loc-id game-state :thief)
        stiletto-loc (gs/get-thing-loc-id game-state :stiletto)]
    (if (= stiletto-loc thief-loc)
      ;; Stiletto is in same room as thief - pick it up
      (-> game-state
          (gs/set-thing-flag :stiletto :ndesc)
          (assoc-in [:objects :stiletto :in] :thief))
      game-state)))

(defn drop-stiletto
  "Drop the stiletto in the current room."
  [game-state]
  (let [here (:here game-state)
        stiletto-in-thief? (= (gs/get-thing-loc-id game-state :stiletto) :thief)]
    (if stiletto-in-thief?
      (-> game-state
          (assoc-in [:objects :stiletto :in] here)
          (gs/unset-thing-flag :stiletto :ndesc))
      game-state)))

(defn deposit-booty
  "Move valuable items from thief to a room.
   ZIL: DEPOSIT-BOOTY routine (1actions.zil lines 1910-1922)"
  [game-state room-id]
  (let [thief-contents (gs/get-contents game-state :thief)
        valuables (filter (fn [obj-id]
                            (and (not (#{:stiletto :large-bag} obj-id))
                                 (pos? (get (gs/get-thing game-state obj-id) :tvalue 0))))
                          thief-contents)]
    (reduce (fn [gs obj-id]
              (-> gs
                  (assoc-in [:objects obj-id :in] room-id)
                  (gs/unset-thing-flag obj-id :invisible)))
            game-state
            valuables)))

;;; ---------------------------------------------------------------------------
;;; THIEF ACTION HANDLER (ROBBER-FUNCTION)
;;; ---------------------------------------------------------------------------

(defn thief-action
  "Thief action handler - ZIL: ROBBER-FUNCTION (1actions.zil line 1960)

   Modes:
   :f-busy?       - Check if thief is recovering stiletto
   :f-dead        - Thief has been killed
   :f-unconscious - Thief knocked unconscious
   :f-conscious   - Thief wakes up
   :f-first?      - Should thief attack first? (20% if visible and thief-here)
   nil            - Normal verb handling"
  [game-state & [mode]]
  (let [prsa (parser-state/get-prsa game-state)
        prso (parser-state/get-prso game-state)
        prsi (parser-state/get-prsi game-state)
        here (:here game-state)
        thief-here? (= (gs/get-thing-loc-id game-state :thief) here)
        stiletto-in-thief? (= (gs/get-thing-loc-id game-state :stiletto) :thief)
        stiletto-in-room? (= (gs/get-thing-loc-id game-state :stiletto) here)
        thief-visible? (not (gs/set-thing-flag? game-state :thief :invisible))
        thief-unconscious? (neg? (get (gs/get-thing game-state :thief) :strength 5))]
    (case mode
      ;; F-BUSY? - Check if thief is recovering stiletto
      :f-busy?
      (cond
        ;; Already has stiletto - not busy
        stiletto-in-thief?
        game-state

        ;; Stiletto in room - pick it up
        stiletto-in-room?
        (-> game-state
            (gs/set-thing-flag :stiletto :ndesc)
            (assoc-in [:objects :stiletto :in] :thief)
            (cond-> thief-here?
              (utils/tell "The robber, somewhat surprised at this turn of events, nimbly retrieves his stiletto.")))

        :else
        game-state)

      ;; F-DEAD - Thief killed
      :f-dead
      (let [in-treasure-room? (= here :treasure-room)]
        (-> game-state
            ;; Drop stiletto
            (drop-stiletto)
            ;; Deposit booty in current room
            (deposit-booty here)
            ;; Disable thief daemon
            (daemon/disable :i-thief)
            ;; If in treasure room, reveal hidden treasures
            (cond-> in-treasure-room?
              (as-> gs
                    (let [room-contents (gs/get-contents gs :treasure-room)
                          hidden-treasures (filter (fn [obj-id]
                                                     (and (not (#{:chalice :thief :adventurer} obj-id))
                                                          (gs/set-thing-flag? gs obj-id :invisible)))
                                                   room-contents)]
                      (if (seq hidden-treasures)
                        (-> (reduce (fn [g obj-id]
                                      (gs/unset-thing-flag g obj-id :invisible))
                                    gs
                                    hidden-treasures)
                            (utils/tell "As the thief dies, the power of his magic decreases, and his treasures reappear."))
                        gs))))))

      ;; F-UNCONSCIOUS - Thief knocked out
      :f-unconscious
      (-> game-state
          (gs/unset-thing-flag :thief :fight)
          ;; Drop stiletto
          (drop-stiletto)
          ;; Update description
          (assoc-in [:objects :thief :ldesc] robber-u-desc)
          ;; Disable daemon while unconscious
          (daemon/disable :i-thief))

      ;; F-CONSCIOUS - Thief wakes up
      :f-conscious
      (-> game-state
          ;; Re-enable daemon
          (daemon/enable :i-thief)
          ;; Recover stiletto if nearby
          (recover-stiletto)
          ;; Update description
          (assoc-in [:objects :thief :ldesc] robber-c-desc)
          ;; If in same room as player, resume fighting
          (cond-> thief-here?
            (-> (gs/set-thing-flag :thief :fight)
                (utils/tell "The robber revives, briefly feigning continued unconsciousness, and, when he sees his moment, scrambles away from you."))))

      ;; F-FIRST? - Should thief strike first? (20% chance if visible)
      :f-first?
      (when (and (get game-state :thief-here)
                 thief-visible?
                 (< (random/rand-int* 100) 20))
        (gs/set-thing-flag game-state :thief :fight))

      ;; Default - verb handling
      nil
      (cond
        ;; TELL - thief ignores conversation
        (= prsa :tell)
        (-> game-state
            (utils/tell "The thief is a strong, silent type.")
            (assoc-in [:parser :p-cont] nil))

        ;; HELLO to unconscious thief
        (and (= prsa :hello) thief-unconscious?)
        (utils/tell game-state "The thief, being temporarily incapacitated, is unable to acknowledge your greeting with his usual graciousness.")

        ;; EXAMINE - detailed description
        (#{:examine :look-inside} prsa)
        (utils/tell game-state "The thief is a slippery character with beady eyes that flit back and forth. He carries, along with an unmistakable arrogance, a large bag over his shoulder and a vicious stiletto, whose blade is aimed menacingly in your direction. I'd watch out if I were you.")

        ;; LISTEN
        (= prsa :listen)
        (utils/tell game-state "The thief says nothing, as you have not been formally introduced.")

        ;; GIVE valuable to thief - thief becomes engrossed
        (and (#{:give :throw} prsa)
             prso
             (not= prso :thief)
             (= prsi :thief))
        (let [obj (gs/get-thing game-state prso)
              tvalue (get obj :tvalue 0)]
          (-> game-state
              ;; If thief was unconscious, revive him
              (cond-> thief-unconscious?
                (-> (assoc-in [:objects :thief :strength]
                              (Math/abs (get (gs/get-thing game-state :thief) :strength 5)))
                    (daemon/enable :i-thief)
                    (recover-stiletto)
                    (assoc-in [:objects :thief :ldesc] robber-c-desc)
                    (utils/tell "Your proposed victim suddenly recovers consciousness.")))
              ;; Move object to thief
              (assoc-in [:objects prso :in] :thief)
              ;; Announce based on value
              (cond->
                (pos? tvalue)
                (-> (assoc :thief-engrossed true)
                    (utils/tell (str "The thief is taken aback by your unexpected generosity, but accepts the " (:desc obj) " and stops to admire its beauty.")))

                (not (pos? tvalue))
                (utils/tell (str "The thief places the " (:desc obj) " in his bag and thanks you politely.")))))

        ;; TAKE/MOVE thief
        (#{:take :move} prsa)
        (utils/tell game-state "Once you got him, what would you do with him?")

        ;; Default - no special handling
        :else
        nil)

      ;; Unknown mode - return unchanged
      game-state)))

;;; ---------------------------------------------------------------------------
;;; STEALING FUNCTIONS
;;; ---------------------------------------------------------------------------

(defn rob
  "Steal valuable items from a container or room.
   ZIL: ROB routine (1actions.zil lines 3989-3999)

   Parameters:
     game-state - Current game state
     what - Container/room ID to steal from
     where - Where to move stolen items (:thief usually)
     prob - Optional probability (0-100) for each item, nil = always steal

   Returns [game-state robbed?] where robbed? is true if anything was stolen."
  [game-state what where & [prob]]
  (let [contents (gs/get-contents game-state what)
        winner (:winner game-state)]
    (reduce
     (fn [[gs robbed?] obj-id]
       (let [obj (gs/get-thing gs obj-id)
             tvalue (get obj :tvalue 0)
             invisible? (gs/set-thing-flag? gs obj-id :invisible)
             sacred? (gs/set-thing-flag? gs obj-id :sacred)]
         (if (and (not invisible?)
                  (not sacred?)
                  (pos? tvalue)
                  (or (nil? prob) (< (random/rand-int* 100) prob)))
           ;; Steal this item
           [(-> gs
                (assoc-in [:objects obj-id :in] where)
                (gs/set-thing-flag obj-id :touch)
                (gs/set-thing-flag obj-id :invisible))
            true]
           ;; Don't steal
           [gs robbed?])))
     [game-state false]
     contents)))

(defn steal-junk
  "Steal worthless takeable items from a room.
   ZIL: STEAL-JUNK routine (1actions.zil lines 3967-3987)

   10% probability to steal each worthless item.
   Always steals stiletto if found."
  [game-state room-id]
  (let [contents (gs/get-contents game-state room-id)
        here (:here game-state)]
    (reduce
     (fn [[gs stolen?] obj-id]
       (let [obj (gs/get-thing gs obj-id)
             tvalue (get obj :tvalue 0)
             takeable? (gs/set-thing-flag? gs obj-id :take)
             invisible? (gs/set-thing-flag? gs obj-id :invisible)
             sacred? (gs/set-thing-flag? gs obj-id :sacred)
             is-stiletto? (= obj-id :stiletto)]
         (if (and (zero? tvalue)
                  takeable?
                  (not sacred?)
                  (not invisible?)
                  (or is-stiletto? (< (random/rand-int* 100) 10)))
           ;; Steal this item
           (let [new-gs (-> gs
                            (assoc-in [:objects obj-id :in] :thief)
                            (gs/set-thing-flag obj-id :touch)
                            (gs/set-thing-flag obj-id :invisible))]
             (if (= room-id here)
               ;; Announce if stealing in player's room
               [(utils/tell new-gs (str "You suddenly notice that the "
                                        (:desc obj) " vanished."))
                true]
               [new-gs stolen?]))
           [gs stolen?])))
     [game-state false]
     contents)))

(defn drop-junk
  "Drop worthless items from thief's inventory.
   ZIL: DROP-JUNK routine (1actions.zil lines 3946-3960)

   30% probability to drop each worthless item.
   Never drops stiletto or large-bag."
  [game-state room-id]
  (let [thief-contents (gs/get-contents game-state :thief)
        here (:here game-state)]
    (reduce
     (fn [[gs announced?] obj-id]
       (let [obj (gs/get-thing gs obj-id)
             tvalue (get obj :tvalue 0)
             protected? (#{:stiletto :large-bag} obj-id)]
         (if (and (not protected?)
                  (zero? tvalue)
                  (< (random/rand-int* 100) 30))
           ;; Drop this item
           (let [new-gs (-> gs
                            (assoc-in [:objects obj-id :in] room-id)
                            (gs/unset-thing-flag obj-id :invisible))]
             (if (and (= room-id here) (not announced?))
               ;; Announce once if dropping in player's room
               [(utils/tell new-gs "The robber, rummaging through his bag, dropped a few items he found valueless.")
                true]
               [new-gs announced?]))
           [gs announced?])))
     [game-state false]
     thief-contents)))

;;; ---------------------------------------------------------------------------
;;; ROOM LIST FOR WANDERING
;;; ---------------------------------------------------------------------------
;;; ZIL iterates through ROOMS object. We use a static list of valid room IDs.

(def wanderable-rooms
  "List of rooms the thief can wander to.
   Excludes :sacred rooms - actual filtering done at runtime to handle dynamic flags."
  [:cellar :troll-room :east-of-chasm :gallery :studio :east-west-passage
   :round-room :ns-passage :chasm-room
   :maze-1 :maze-2 :maze-3 :maze-4 :maze-5 :maze-6 :maze-7 :maze-8
   :maze-9 :maze-10 :maze-11 :maze-12 :maze-13 :maze-14 :maze-15
   :dead-end-1 :dead-end-2 :dead-end-3 :dead-end-4
   :grating-room :cyclops-room :treasure-room])

(defn valid-room-for-thief?
  "Check if a room is valid for thief to enter.
   ZIL: <AND <NOT <FSET? .RM ,SACREDBIT>> <FSET? .RM ,RLANDBIT>>"
  [game-state room-id]
  (let [room (gs/get-thing game-state room-id)]
    (and room
         (not (gs/set-thing-flag? game-state room-id :sacred)))))

(defn find-next-room
  "Find the next valid room for thief to wander to.
   ZIL: Loop through ROOMS, find one without SACREDBIT and with RLANDBIT."
  [game-state current-room-id]
  (let [room-ids (keys (:rooms game-state))
        room-vec (vec room-ids)
        current-idx (.indexOf room-vec current-room-id)
        n (count room-vec)]
    ;; Start from current position and loop through all rooms
    (loop [i 0]
      (when (< i n)
        (let [idx (mod (+ current-idx 1 i) n)
              room-id (nth room-vec idx)]
          (if (valid-room-for-thief? game-state room-id)
            room-id
            (recur (inc i))))))))

;;; ---------------------------------------------------------------------------
;;; TREASURE ROOM BEHAVIOR
;;; ---------------------------------------------------------------------------

(defn hack-treasures
  "Reveal hidden treasures in treasure room when thief is alone.
   ZIL: HACK-TREASURES (1actions.zil line 2140)

   Called when thief is in treasure room alone - unhides all valuable items."
  [game-state]
  (let [contents (gs/get-contents game-state :treasure-room)]
    (reduce
     (fn [gs obj-id]
       (let [obj (gs/get-thing gs obj-id)
             tvalue (get obj :tvalue 0)]
         (if (and (pos? tvalue)
                  (gs/set-thing-flag? gs obj-id :invisible))
           (gs/unset-thing-flag gs obj-id :invisible)
           gs)))
     game-state
     contents)))

(defn thief-in-treasure
  "Handle thief defending treasure room when player enters.
   ZIL: THIEF-IN-TREASURE (1actions.zil line 2104)

   - Moves thief to treasure room
   - Makes him visible
   - Sets thief-here flag
   - Hides all valuable items in room
   - Sets fight flag"
  [game-state]
  (let [contents (gs/get-contents game-state :treasure-room)
        ;; Hide all valuables
        gs (reduce
            (fn [state obj-id]
              (let [obj (gs/get-thing state obj-id)
                    tvalue (get obj :tvalue 0)]
                (if (and (pos? tvalue)
                         (not= obj-id :chalice))  ; Chalice is special in ZIL
                  (gs/set-thing-flag state obj-id :invisible)
                  state)))
            game-state
            contents)]
    (-> gs
        ;; Move thief to treasure room
        (assoc-in [:objects :thief :in] :treasure-room)
        ;; Make visible
        (gs/unset-thing-flag :thief :invisible)
        ;; Set thief-here
        (assoc :thief-here true)
        ;; Set fight flag
        (gs/set-thing-flag :thief :fight))))

(defn treasure-room-action
  "Treasure room action handler.
   ZIL: TREASURE-ROOM-FCN (1actions.zil line 2151)

   On M-ENTER:
   - If thief daemon is enabled and player not dead:
   - If thief not in room: announce thief rushing to defend
   - Call hack-treasures when thief alone"
  [game-state rarg]
  (case rarg
    :m-enter
    (let [dead? (:dead game-state)
          thief-daemon-enabled? (and (get-in game-state [:daemons :i-thief])
                                     (get-in game-state [:daemons :i-thief :enabled] true))
          thief-here? (= (gs/get-thing-loc-id game-state :thief) :treasure-room)]
      (if (and thief-daemon-enabled? (not dead?))
        (if thief-here?
          ;; Thief already here - just set up fight
          (-> game-state
              (gs/unset-thing-flag :thief :invisible)
              (gs/set-thing-flag :thief :fight)
              (assoc :thief-here true)
              (thief-in-treasure))
          ;; Thief not here - he rushes to defend
          (-> game-state
              (utils/tell "You hear a scream of anguish as you violate the robber's hideaway. Using passages unknown to you, he rushes to its defense.")
              (thief-in-treasure)))
        game-state))

    ;; Default - no special handling
    game-state))

;;; ---------------------------------------------------------------------------
;;; THIEF-VS-ADVENTURER
;;; ---------------------------------------------------------------------------

(defn winning?
  "Check if a combatant is winning (has more strength remaining).
   ZIL: WINNING? (1actions.zil)"
  [game-state combatant-id]
  (let [player (:winner game-state)
        player-strength (get-in game-state [:objects player :strength] 0)
        combatant-strength (get-in game-state [:objects combatant-id :strength] 0)]
    (> combatant-strength player-strength)))

(defn thief-vs-adventurer
  "Handle thief encountering the player.
   ZIL: THIEF-VS-ADVENTURER (1actions.zil line 1777)

   Returns [game-state finished?] where finished? is true if thief's turn is done.

   Scenarios:
   1. First encounter (30% appear): dramatic message, set :thief-here
   2. Losing fight: retreat and become invisible
   3. Rob player: steal valuables from inventory/room
   4. Leave disgusted: 30% chance to leave without stealing"
  [game-state thief-visible?]
  (let [here (:here game-state)
        dead? (:dead game-state)
        thief-here? (:thief-here game-state)
        has-stiletto? (= (gs/get-thing-loc-id game-state :stiletto) :thief)
        fighting? (gs/set-thing-flag? game-state :thief :fight)
        winner (:winner game-state)]

    (cond
      ;; Skip if player dead and in treasure room
      (and dead? (= here :treasure-room))
      [(trace/trace-thief game-state "VS-ADV: Skip (player dead in treasure room)") false]

      ;; Thief not yet announced
      (not thief-here?)
      (cond
        ;; 30% chance to appear (if invisible and has stiletto)
        (and (not dead?)
             (not thief-visible?)
             (< (random/rand-int* 100) 30)
             has-stiletto?)
        [(-> game-state
             (trace/trace-thief "VS-ADV: Appearing dramatically!")
             (gs/unset-thing-flag :thief :invisible)
             (assoc :thief-here true)
             (utils/tell "Someone carrying a large bag is casually leaning against one of the walls here. He does not speak, but it is clear from his aspect that the bag will be taken only over his dead body."))
         true]

        ;; If visible, fighting, and losing - retreat
        (and thief-visible?
             fighting?
             (not (winning? game-state :thief)))
        [(-> game-state
             (trace/trace-thief "VS-ADV: Losing fight, retreating!")
             (utils/tell "Your opponent, determining discretion to be the better part of valor, decides to terminate this little contretemps. With a rueful nod of his head, he steps backward into the gloom and disappears.")
             (gs/set-thing-flag :thief :invisible)
             (gs/unset-thing-flag :thief :fight)
             (recover-stiletto))
         true]

        ;; If fighting, 90% stay and fight
        (and thief-visible? fighting? (< (random/rand-int* 100) 90))
        [(trace/trace-thief game-state "VS-ADV: Standing ground to fight") false]

        ;; If visible, 30% leave disgusted
        (and thief-visible? (< (random/rand-int* 100) 30))
        [(-> game-state
             (trace/trace-thief "VS-ADV: Leaving disgusted (nothing to steal)")
             (utils/tell "The holder of the large bag just left, looking disgusted. Fortunately, he took nothing.")
             (gs/set-thing-flag :thief :invisible)
             (recover-stiletto))
         true]

        ;; 70% chance to do nothing
        (< (random/rand-int* 100) 70)
        [(trace/trace-thief game-state "VS-ADV: Doing nothing (70% roll)") false]

        ;; Otherwise, try to rob
        (not dead?)
        (let [gs (trace/trace-thief game-state "VS-ADV: Attempting to rob...")
              ;; Try robbing room first, then player
              [gs room-robbed?] (rob gs here :thief 100)
              [gs player-robbed?] (if room-robbed?
                                    [gs false]
                                    (rob gs winner :thief nil))
              robbed-what (cond
                            room-robbed? :room
                            player-robbed? :player
                            :else nil)
              gs (trace/trace-thief gs (str "VS-ADV: Rob result - " (or robbed-what "nothing")))]
          (if (and robbed-what (not thief-visible?))
            ;; Robbed while invisible
            [(-> gs
                 (assoc :thief-here true)
                 (utils/tell
                  (str "A seedy-looking individual with a large bag just wandered through the room. On the way through, he quietly abstracted some valuables from "
                       (if (= robbed-what :room) "the room" "your possession")
                       ", mumbling something about \"Doing unto others before...\"")))
             true]
            (if thief-visible?
              ;; Visible thief leaving
              (let [gs (recover-stiletto gs)]
                (if robbed-what
                  [(-> gs
                       (utils/tell
                        (str "The thief just left, still carrying his large bag. You may not have noticed that he "
                             (if (= robbed-what :player)
                               "robbed you blind first."
                               "appropriated the valuables in the room.")))
                       (gs/set-thing-flag :thief :invisible))
                   true]
                  [(-> gs
                       (utils/tell "The thief, finding nothing of value, left disgusted.")
                       (gs/set-thing-flag :thief :invisible))
                   true]))
              ;; Invisible, nothing to rob
              [(utils/tell gs "A \"lean and hungry\" gentleman just wandered through, carrying a large bag. Finding nothing of value, he left disgruntled.")
               true])))

        :else
        [(trace/trace-thief game-state "VS-ADV: No action taken") false])

      ;; Thief already announced (here?)
      :else
      (if (and thief-visible? (< (random/rand-int* 100) 30))
        ;; 30% chance to rob and leave
        (let [gs (trace/trace-thief game-state "VS-ADV: Announced, trying to rob and leave...")
              [gs room-robbed?] (rob gs here :thief 100)
              [gs player-robbed?] (if room-robbed? [gs false] (rob gs winner :thief nil))
              robbed-what (cond room-robbed? :room player-robbed? :player :else nil)
              gs (recover-stiletto gs)]
          (if robbed-what
            [(-> gs
                 (trace/trace-thief (str "VS-ADV: Robbed " (name robbed-what) " and leaving"))
                 (utils/tell
                  (str "The thief just left, still carrying his large bag. You may not have noticed that he "
                       (if (= robbed-what :player)
                         "robbed you blind first."
                         "appropriated the valuables in the room.")))
                 (gs/set-thing-flag :thief :invisible)
                 (assoc :thief-here false))
             true]
            [(-> gs
                 (trace/trace-thief "VS-ADV: Nothing to steal, leaving disgusted")
                 (utils/tell "The thief, finding nothing of value, left disgusted.")
                 (gs/set-thing-flag :thief :invisible)
                 (assoc :thief-here false))
             true]))
        [(trace/trace-thief game-state "VS-ADV: Announced, staying (70% roll)") false]))))

;;; ---------------------------------------------------------------------------
;;; I-THIEF DAEMON
;;; ---------------------------------------------------------------------------

(defn i-thief
  "Thief daemon - moves between rooms and interacts with environment.
   ZIL: I-THIEF (1actions.zil line 3903)

   Behavior:
   - If in same room as player (dark room, no troll): run thief-vs-adventurer
   - If thief is visible and not with player, become invisible
   - If room has been visited (:touch flag), steal valuables (75% prob) and junk
   - Move to next valid room (no :sacred flag)
   - Clear :fight flag when leaving
   - Recover stiletto when moving
   - Drop junk items when not in treasure room"
  [game-state]
  (let [thief-loc (gs/get-thing-loc-id game-state :thief)
        here (:here game-state)
        thief-visible? (not (gs/set-thing-flag? game-state :thief :invisible))
        thief-in-player-room? (= thief-loc here)
        in-treasure-room? (= thief-loc :treasure-room)
        room-touched? (gs/set-thing-flag? game-state thief-loc :touch)
        room-lit? (gs/set-thing-flag? game-state here :on)  ; ONBIT - naturally lit
        troll-here? (= (gs/get-thing-loc-id game-state :troll) here)
        gs (trace/trace-thief game-state
                              (str "I-THIEF: at " thief-loc
                                   (when thief-in-player-room? " [WITH PLAYER]")
                                   (when thief-visible? " [VISIBLE]")
                                   (when in-treasure-room? " [IN LAIR]")))]

    ;; Skip if player is dead
    (if (:dead game-state)
      (trace/trace-thief gs "Skipping - player dead")

      ;; If thief is in treasure room alone, deposit booty and hack treasures
      ;; NOTE: Unlike player encounters, this does NOT return early - thief continues
      ;; to movement logic below. ZIL doesn't RTRUE here, it falls through.
      (let [gs (if (and in-treasure-room? (not thief-in-player-room?))
                 (-> gs
                     (trace/trace-thief "In lair alone - depositing booty and revealing treasures")
                     (deposit-booty :treasure-room)
                     (hack-treasures))
                 gs)]

      ;; Check for player encounter
      (if (and thief-in-player-room?
               (not room-lit?)  ; Only in dark rooms
               (not troll-here?))  ; Troll must not be present
        ;; Handle thief-vs-adventurer encounter
        (let [gs (trace/trace-thief gs "Encounter with player!")
              [gs finished?] (thief-vs-adventurer gs thief-visible?)]
          (if finished?
            (trace/trace-thief gs "Encounter resolved")
            ;; If thief became invisible during encounter, he might leave
            (if (gs/set-thing-flag? gs :thief :invisible)
              (trace/trace-thief gs "Staying invisible")
              gs)))

        ;; Normal wandering behavior
        (let [;; If thief is visible and not in same room as player, become invisible
              gs (if (and thief-visible? (not thief-in-player-room?))
                   (-> gs
                       (trace/trace-thief "Making self invisible")
                       (gs/set-thing-flag :thief :invisible))
                   gs)

              ;; Steal from room if it has been visited (and thief is not with player)
              [gs robbed?] (if (and room-touched? (not thief-in-player-room?))
                             (let [[new-gs was-robbed?] (rob gs thief-loc :thief 75)]
                               (if was-robbed?
                                 [(trace/trace-thief new-gs (str "Robbed valuables from " thief-loc)) true]
                                 [new-gs false]))
                             [gs false])

              ;; Also try to steal junk
              [gs stolen?] (if (and room-touched? (not thief-in-player-room?))
                             (let [[new-gs was-stolen?] (steal-junk gs thief-loc)]
                               (if was-stolen?
                                 [(trace/trace-thief new-gs (str "Stole junk from " thief-loc)) true]
                                 [new-gs false]))
                             [gs false])

              ;; Find next room to move to
              next-room (find-next-room gs thief-loc)]

          (if next-room
            ;; Move to next room
            (let [gs (trace/trace-thief gs (str "Moving " thief-loc " -> " next-room))
                  moved-gs (-> gs
                               ;; Recover stiletto before moving
                               (recover-stiletto)
                               ;; Move thief
                               (assoc-in [:objects :thief :in] next-room)
                               ;; Clear fight flag
                               (gs/unset-thing-flag :thief :fight)
                               ;; Become invisible
                               (gs/set-thing-flag :thief :invisible)
                               ;; Clear thief-here flag
                               (assoc :thief-here false))]
              ;; Drop junk if not in treasure room
              (if (not= next-room :treasure-room)
                (let [[dropped-gs dropped?] (drop-junk moved-gs next-room)]
                  (if dropped?
                    (trace/trace-thief dropped-gs "Dropped some junk")
                    dropped-gs))
                moved-gs))
            ;; No valid room found - stay put
            (trace/trace-thief gs "No valid room found - staying put"))))))))  ; Extra paren for treasure room let
