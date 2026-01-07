(ns clork.combat
  "Combat system - ZIL-equivalent fight mechanics.

   Implements the combat system from Zork I, closely following the ZIL source
   in 1actions.zil (lines 3259-3860).

   Combat flow:
   1. Player attacks with ATTACK/KILL command -> v-attack -> hero-blow
   2. Each turn, I-FIGHT daemon checks for active villains
   3. Active villains counter-attack via villain-blow
   4. Results determined by lookup in defense tables based on strengths

   ZIL Reference: 1actions.zil lines 3259-3860"
  (:require [clork.random :as random]
            [clork.game-state :as gs]
            [clork.verbs-health :as health]
            [clork.utils :as utils]
            [clork.death :as death]))

;;; ---------------------------------------------------------------------------
;;; COMBAT RESULT CONSTANTS
;;; ---------------------------------------------------------------------------
;;; ZIL: 1actions.zil lines 3259-3267

(def missed 1)        ; Attacker misses
(def unconscious 2)   ; Defender unconscious
(def killed 3)        ; Defender dead
(def light-wound 4)   ; Defender lightly wounded (-1 strength)
(def serious-wound 5) ; Defender seriously wounded (-2 strength)
(def stagger 6)       ; Defender staggered (miss turn)
(def lose-weapon 7)   ; Defender loses weapon
(def hesitate 8)      ; Hesitates (miss on free swing)
(def sitting-duck 9)  ; Sitting duck (instant kill)

(def result-names
  "Human-readable names for combat results."
  {missed "missed"
   unconscious "unconscious"
   killed "killed"
   light-wound "light wound"
   serious-wound "serious wound"
   stagger "stagger"
   lose-weapon "lose weapon"
   hesitate "hesitate"
   sitting-duck "sitting duck"})

;;; ---------------------------------------------------------------------------
;;; VILLAIN TABLE STRUCTURE
;;; ---------------------------------------------------------------------------
;;; ZIL: VILLAINS global table structure (1actions.zil line 3814)
;;;
;;; Each villain entry has:
;;;   :villain-id   - The villain object keyword
;;;   :best-weapon  - Weapon that gives advantage against this villain
;;;   :best-adv     - Advantage points when using best weapon (usually 1)
;;;   :wake-prob    - Probability of waking if unconscious (starts 0, +25 each check)
;;;   :messages     - Keyword for villain's melee message table

(def v-villain 0)   ; Index: villain object
(def v-best 1)      ; Index: best weapon against
(def v-best-adv 2)  ; Index: advantage when using best weapon
(def v-prob 3)      ; Index: probability of waking
(def v-msgs 4)      ; Index: message table

;;; ---------------------------------------------------------------------------
;;; DEFENSE RESULT TABLES
;;; ---------------------------------------------------------------------------
;;; ZIL: DEF1, DEF2A, DEF2B, DEF3A, DEF3B, DEF3C (1actions.zil lines 3271-3312)
;;;
;;; Tables indexed 0-9 (originally 0-12 in ZIL, but only 10 used via RANDOM 9)
;;; Results weighted by position in table.

;; DEF1: Weak defender (strength 1)
;; 4 missed, 2 stagger, 2 unconscious, 5 killed (13 entries, use first 10)
(def def1
  [missed missed missed missed
   stagger stagger
   unconscious unconscious
   killed killed killed killed killed])

;; DEF2A: Moderate defender, weak attack
;; 5 missed, 2 stagger, 2 light-wound, 1 unconscious
(def def2a
  [missed missed missed missed missed
   stagger stagger
   light-wound light-wound
   unconscious])

;; DEF2B: Moderate defender, strong attack
;; 3 missed, 2 stagger, 3 light-wound, 1 unconscious, 3 killed
(def def2b
  [missed missed missed
   stagger stagger
   light-wound light-wound light-wound
   unconscious
   killed killed killed])

;; DEF3A: Strong defender, weak attack
;; 5 missed, 2 stagger, 2 light-wound, 2 serious-wound
(def def3a
  [missed missed missed missed missed
   stagger stagger
   light-wound light-wound
   serious-wound serious-wound])

;; DEF3B: Strong defender, moderate attack
;; 3 missed, 2 stagger, 3 light-wound, 3 serious-wound
(def def3b
  [missed missed missed
   stagger stagger
   light-wound light-wound light-wound
   serious-wound serious-wound serious-wound])

;; DEF3C: Strong defender, strong attack
;; 1 missed, 2 stagger, 4 light-wound, 3 serious-wound
(def def3c
  [missed
   stagger stagger
   light-wound light-wound light-wound light-wound
   serious-wound serious-wound serious-wound])

;; DEF-RES tables: select subtable based on attack strength
;; ZIL: DEF1-RES, DEF2-RES, DEF3-RES (lines 3314-3330)
(def def1-res [def1 def1 def1])       ; DEF1 for all attack levels
(def def2-res [def2a def2b def2b])    ; DEF2A for weak, DEF2B for strong
(def def3-res [def3a def3a def3b def3b def3c]) ; Varies with attack differential

;;; ---------------------------------------------------------------------------
;;; MESSAGE TABLES
;;; ---------------------------------------------------------------------------
;;; ZIL: HERO-MELEE, TROLL-MELEE (1actions.zil lines 3624-3743)
;;;
;;; Message tables are nested vectors indexed by result type (1-9).
;;; Each result type has multiple message variants for variety.
;;; Special markers:
;;;   :f-wep - Substitute weapon name
;;;   :f-def - Substitute defender name

(def hero-melee
  "Messages for player attacks. ZIL: HERO-MELEE (1actions.zil line 3624)
   Indexed by result constant (1-9), each containing variant messages."
  {missed
   [["Your " :f-wep " misses the " :f-def " by an inch."]
    ["A good slash, but it misses the " :f-def " by a mile."]
    ["You charge, but the " :f-def " jumps nimbly aside."]
    ["Clang! Crash! The " :f-def " parries."]
    ["A quick stroke, but the " :f-def " is on guard."]
    ["A good stroke, but it's too slow; the " :f-def " dodges."]]

   unconscious
   [["Your " :f-wep " crashes down, knocking the " :f-def " into dreamland."]
    ["The " :f-def " is battered into unconsciousness."]
    ["A furious exchange, and the " :f-def " is knocked out!"]
    ["The haft of your " :f-wep " knocks out the " :f-def "."]
    ["The " :f-def " is knocked out!"]]

   killed
   [["It's curtains for the " :f-def " as your " :f-wep " removes his head."]
    ["The fatal blow strikes the " :f-def " square in the heart: He dies."]
    ["The " :f-def " takes a fatal blow and slumps to the floor dead."]]

   light-wound
   [["The " :f-def " is struck on the arm; blood begins to trickle down."]
    ["Your " :f-wep " pinks the " :f-def " on the wrist, but it's not serious."]
    ["Your stroke lands, but it was only the flat of the blade."]
    ["The blow lands, making a shallow gash in the " :f-def "'s arm!"]]

   serious-wound
   [["The " :f-def " receives a deep gash in his side."]
    ["A savage blow on the thigh! The " :f-def " is stunned but can still fight!"]
    ["Slash! Your blow lands! That one hit an artery, it could be serious!"]
    ["Slash! Your stroke connects! This could be serious!"]]

   stagger
   [["The " :f-def " is staggered, and drops to his knees."]
    ["The " :f-def " is momentarily disoriented and can't fight back."]
    ["The force of your blow knocks the " :f-def " back, stunned."]
    ["The " :f-def " is confused and can't fight back."]
    ["The quickness of your thrust knocks the " :f-def " back, stunned."]]

   lose-weapon
   [["The " :f-def "'s weapon is knocked to the floor, leaving him unarmed."]
    ["The " :f-def " is disarmed by a subtle feint past his guard."]]})

(def troll-melee
  "Messages for troll attacks. ZIL: TROLL-MELEE (1actions.zil line 3702)
   Indexed by result constant (1-9), each containing variant messages."
  {missed
   [["The troll swings his axe, but it misses."]
    ["The troll's axe barely misses your ear."]
    ["The axe sweeps past as you jump aside."]
    ["The axe crashes against the rock, throwing sparks!"]]

   unconscious
   [["The flat of the troll's axe hits you delicately on the head, knocking you out."]]

   killed
   [["The troll neatly removes your head."]
    ["The troll's axe stroke cleaves you from the nave to the chops."]
    ["The troll's axe removes your head."]]

   light-wound
   [["The axe gets you right in the side. Ouch!"]
    ["The flat of the troll's axe skins across your forearm."]
    ["The troll's swing almost knocks you over as you barely parry in time."]
    ["The troll swings his axe, and it nicks your arm as you dodge."]]

   serious-wound
   [["The troll charges, and his axe slashes you on your " :f-wep " arm."]
    ["An axe stroke makes a deep wound in your leg."]
    ["The troll's axe swings down, gashing your shoulder."]]

   stagger
   [["The troll hits you with a glancing blow, and you are momentarily stunned."]
    ["The troll swings; the blade turns on your armor but crashes broadside into your head."]
    ["You stagger back under a hail of axe strokes."]
    ["The troll's mighty blow drops you to your knees."]]

   lose-weapon
   [["The axe hits your " :f-wep " and knocks it spinning."]
    ["The troll swings, you parry, but the force of his blow knocks your " :f-wep " away."]
    ["The axe knocks your " :f-wep " out of your hand. It falls to the floor."]]

   hesitate
   [["The troll hesitates, fingering his axe."]
    ["The troll scratches his head ruminatively: Might you be magically protected, he wonders?"]]

   sitting-duck
   [["Conquering his fears, the troll puts you to death."]]})

(def thief-melee
  "Messages for thief attacks. ZIL: THIEF-MELEE (1actions.zil line 3748)
   Indexed by result constant (1-9), each containing variant messages."
  {missed
   [["The thief stabs nonchalantly with his stiletto and misses."]
    ["You dodge as the thief comes in low."]
    ["You parry a lightning thrust, and the thief salutes you with a grim nod."]
    ["The thief tries to sneak past your guard, but you twist away."]]

   unconscious
   [["Shifting in the midst of a thrust, the thief knocks you unconscious with the haft of his stiletto."]
    ["The thief knocks you out."]]

   killed
   [["Finishing you off, the thief inserts his blade into your heart."]
    ["The thief comes in from the side, feints, and inserts the blade into your ribs."]
    ["The thief bows formally, raises his stiletto, and with a wry grin, ends the battle and your life."]]

   light-wound
   [["A quick thrust pinks your left arm, and blood starts to trickle down."]
    ["The thief draws blood, raking his stiletto across your arm."]
    ["The stiletto flashes faster than you can follow, and blood wells from your leg."]
    ["The thief slowly approaches, strikes like a snake, and leaves you wounded."]]

   serious-wound
   [["The thief strikes like a snake! The resulting wound is serious."]
    ["The thief stabs a deep cut in your upper arm."]
    ["The stiletto touches your forehead, and the blood obscures your vision."]
    ["The thief strikes at your wrist, and suddenly your grip is slippery with blood."]]

   stagger
   [["The butt of his stiletto cracks you on the skull, and you stagger back."]
    ["The thief rams the haft of his blade into your stomach, leaving you out of breath."]
    ["The thief attacks, and you fall back desperately."]]

   lose-weapon
   [["A long, theatrical slash. You catch it on your " :f-wep ", but the thief twists his knife, and the " :f-wep " goes flying."]
    ["The thief neatly flips your " :f-wep " out of your hands, and it drops to the floor."]
    ["You parry a low thrust, and your " :f-wep " slips out of your hand."]]

   hesitate
   [["The thief, a man of superior breeding, pauses for a moment to consider the propriety of finishing you off."]
    ["The thief amuses himself by searching your pockets."]
    ["The thief entertains himself by rifling your pack."]]

   sitting-duck
   [["The thief, forgetting his essentially genteel upbringing, cuts your throat."]
    ["The thief, a pragmatist, dispatches you as a threat to his livelihood."]]})

(def cyclops-melee
  "Messages for cyclops attacks. ZIL: CYCLOPS-MELEE (1actions.zil line 3667)
   Indexed by result constant (1-9), each containing variant messages."
  {missed
   [["The Cyclops misses, but the backwash almost knocks you over."]
    ["The Cyclops rushes you, but runs into the wall."]]

   unconscious
   [["The Cyclops sends you crashing to the floor, unconscious."]]

   killed
   [["The Cyclops breaks your neck with a massive smash."]]

   light-wound
   [["A quick punch, but it was only a glancing blow."]
    ["A glancing blow from the Cyclops' fist."]]

   serious-wound
   [["The monster smashes his huge fist into your chest, breaking several ribs."]
    ["The Cyclops almost knocks the wind out of you with a quick punch."]]

   stagger
   [["The Cyclops lands a punch that knocks the wind out of you."]
    ["Heedless of your weapons, the Cyclops tosses you against the rock wall of the room."]]

   lose-weapon
   [["The Cyclops grabs your " :f-wep ", tastes it, and throws it to the ground in disgust."]
    ["The monster grabs you on the wrist, squeezes, and you drop your " :f-wep " in pain."]]

   hesitate
   [["The Cyclops seems unable to decide whether to broil or stew his dinner."]]

   sitting-duck
   [["The Cyclops, no sportsman, dispatches his unconscious victim."]]})

;;; ---------------------------------------------------------------------------
;;; STRENGTH CALCULATIONS
;;; ---------------------------------------------------------------------------
;;; ZIL: FIGHT-STRENGTH (already in verbs-health), VILLAIN-STRENGTH (1actions.zil line 3396)

(defn villain-strength
  "Calculate a villain's defense strength.

   ZIL: VILLAIN-STRENGTH (1actions.zil line 3396)
   Returns the villain's P?STRENGTH property, potentially modified:
   - If player is using villain's best weapon against them, reduce defense
   - Special: Thief's strength capped at 2 if THIEF-ENGROSSED (not implemented yet)

   Parameters:
     game-state - Current game state
     villain-entry - Villain entry map with :villain-id, :best-weapon, :best-adv
     weapon-id - Weapon player is using (or nil)"
  [game-state villain-entry weapon-id]
  (let [villain-id (:villain-id villain-entry)
        base-strength (get-in game-state [:objects villain-id :strength] 0)]
    ;; If villain is unconscious (negative strength), return as-is
    (if (neg? base-strength)
      base-strength
      ;; Check for weapon advantage
      (let [best-weapon (:best-weapon villain-entry)
            best-adv (:best-adv villain-entry 1)]
        (if (and weapon-id
                 (gs/set-thing-flag? game-state weapon-id :weapon)
                 (= best-weapon weapon-id))
          ;; Player using villain's best weapon - reduce defense
          (max 1 (- base-strength best-adv))
          base-strength)))))

(defn find-weapon
  "Find a weapon in an object's inventory.

   ZIL: FIND-WEAPON (1actions.zil line 3414)
   Searches for an object with WEAPONBIT in the owner's inventory.
   Returns the first weapon found, or nil.

   Used to determine what weapon a villain has, or if player has a weapon."
  [game-state owner-id]
  (let [objects (:objects game-state)]
    (->> objects
         vals
         (filter (fn [obj]
                   (and (= (gs/get-thing-loc-id game-state (:id obj)) owner-id)
                        (gs/set-thing-flag? game-state (:id obj) :weapon))))
         first
         :id)))

;;; ---------------------------------------------------------------------------
;;; COMBAT RESOLUTION
;;; ---------------------------------------------------------------------------
;;; ZIL: Table selection and result rolling from HERO-BLOW/VILLAIN-BLOW

(defn select-result-table
  "Select the appropriate result table based on defender and attacker strengths.

   ZIL: Table selection logic in HERO-BLOW (1actions.zil lines 3532-3556)

   For defender strength 1: Use DEF1
   For defender strength 2: Use DEF2A (weak attack) or DEF2B (strong attack)
   For defender strength 3+: Use DEF3A/B/C based on attack differential

   Returns [table attack-index] where attack-index selects which variant."
  [defender-strength attacker-strength]
  (cond
    ;; DEF1: Weak defender (strength 1)
    (<= defender-strength 1)
    (let [att-idx (cond
                    (> attacker-strength 2) 2  ; Strong attack
                    :else 0)]                   ; Weak/moderate
      [(nth def1-res (min att-idx (dec (count def1-res)))) att-idx])

    ;; DEF2: Moderate defender (strength 2)
    (= defender-strength 2)
    (let [att-idx (cond
                    (> attacker-strength 3) 1  ; Strong attack -> DEF2B
                    :else 0)]                   ; Weak attack -> DEF2A
      [(nth def2-res att-idx) att-idx])

    ;; DEF3: Strong defender (strength 3+)
    :else
    (let [;; Calculate attack differential
          diff (- attacker-strength defender-strength)
          ;; Map differential to index: -2 or less -> 0, -1 -> 1, 0 -> 2, 1 -> 3, 2+ -> 4
          att-idx (cond
                    (<= diff -2) 0
                    (= diff -1) 1
                    (= diff 0) 2
                    (= diff 1) 3
                    :else 4)]
      [(nth def3-res (min att-idx (dec (count def3-res)))) att-idx])))

(defn roll-combat-result
  "Roll for combat result using the given table.

   ZIL: <GET .TBL <- <RANDOM 9> 1>> (random 1-9, then subtract 1 for 0-based index)
   Note: ZIL tables have 10-13 entries but only first 9 used via RANDOM 9.

   Returns the result constant (missed, killed, etc.)"
  [table]
  (let [idx (random/rand-int* (min 9 (count table)))]
    (nth table idx missed)))

(defn apply-out-modifier
  "Apply OUT? modifier to combat result.

   ZIL: OUT? parameter in VILLAIN-BLOW (1actions.zil lines 3462-3470)
   When the player is 'out' (unconscious or staggered), some results change:
   - STAGGER becomes HESITATE (can't capitalize on miss)
   - UNCONSCIOUS becomes SITTING-DUCK (easy kill)

   Also handles weapon loss with 25% probability on STAGGER."
  [result defender-has-weapon? out?]
  (cond
    ;; If out, STAGGER -> HESITATE, UNCONSCIOUS -> SITTING-DUCK
    out?
    (cond
      (= result stagger) hesitate
      (= result unconscious) sitting-duck
      :else result)

    ;; STAGGER has 25% chance to become LOSE-WEAPON if defender armed
    (and (= result stagger) defender-has-weapon?)
    (if (< (random/rand-int* 4) 1)
      lose-weapon
      result)

    :else result))

;;; ---------------------------------------------------------------------------
;;; MESSAGE FORMATTING
;;; ---------------------------------------------------------------------------
;;; ZIL: REMARK routine (1actions.zil line 3376)

(defn format-combat-message
  "Format a combat message, substituting weapon and defender names.

   ZIL: REMARK routine (1actions.zil line 3376)
   Processes message template, replacing:
   - :f-wep with weapon description
   - :f-def with defender description

   message-parts - Vector of strings and keywords
   defender-name - Name of the defender
   weapon-name - Name of the weapon (or nil)"
  [message-parts defender-name weapon-name]
  (let [weapon-name (or weapon-name "weapon")]
    (->> message-parts
         (map (fn [part]
                (cond
                  (= part :f-wep) weapon-name
                  (= part :f-def) defender-name
                  :else part)))
         (apply str))))

(defn select-combat-message
  "Select a random message variant for a combat result.

   message-table - Map of result -> variants (e.g., hero-melee, troll-melee)
   result - Combat result constant
   defender-name - Name of defender
   weapon-name - Name of weapon"
  [message-table result defender-name weapon-name]
  (if-let [variants (get message-table result)]
    (let [variant (random/rand-nth* variants)]
      (format-combat-message variant defender-name weapon-name))
    ;; Fallback if no message defined
    (str "Combat result: " (get result-names result "unknown"))))

;;; ---------------------------------------------------------------------------
;;; DAMAGE APPLICATION
;;; ---------------------------------------------------------------------------
;;; ZIL: WINNER-RESULT, VILLAIN-RESULT (1actions.zil lines 3565-3595)

(defn apply-wound
  "Apply wound damage to a combatant's strength.

   game-state - Current game state
   target-id - Object id to wound
   wound-amount - Amount to reduce strength (positive number)"
  [game-state target-id wound-amount]
  (update-in game-state [:objects target-id :strength]
             (fn [s] (- (or s 0) wound-amount))))

(defn drop-weapon
  "Drop a weapon from combatant's inventory to the current room.

   game-state - Current game state
   owner-id - Object that holds the weapon
   weapon-id - Weapon to drop"
  [game-state owner-id weapon-id]
  (if weapon-id
    (assoc-in game-state [:objects weapon-id :in] (:here game-state))
    game-state))

(defn get-thing-name
  "Get the description of a thing for combat messages."
  [game-state thing-id]
  (or (get-in game-state [:objects thing-id :desc])
      (get-in game-state [:rooms thing-id :desc])
      (name thing-id)))

;;; ---------------------------------------------------------------------------
;;; WINNER-RESULT - Apply combat result to player
;;; ---------------------------------------------------------------------------
;;; ZIL: WINNER-RESULT (1actions.zil line 3565)

(defn winner-result
  "Apply combat result to the player.

   ZIL: WINNER-RESULT (1actions.zil line 3565-3576)

   Updates player's P?STRENGTH and handles death via JIGS-UP.
   Negative strength indicates wounds; when too negative, player dies."
  [game-state new-def result orig-def]
  (let [winner (:winner game-state)
        ;; Calculate wound modifier (difference from original defense)
        wound-modifier (if (zero? new-def)
                         -10000  ; Fatal
                         (- new-def orig-def))
        gs (update-in game-state [:objects winner :strength]
                      (fn [s] (+ (or s 0) wound-modifier)))]
    (if (zero? new-def)
      ;; Player died - call JIGS-UP with death message
      (death/jigs-up gs "It appears that that last blow was too much for you. I'm afraid you are dead.")
      gs)))

;;; ---------------------------------------------------------------------------
;;; VILLAIN-BLOW - Villain attacks player
;;; ---------------------------------------------------------------------------
;;; ZIL: VILLAIN-BLOW (1actions.zil line 3426)

(defn villain-blow
  "Villain attacks the player.

   ZIL: VILLAIN-BLOW (1actions.zil line 3426-3487)

   Flow:
   1. Clear player's STAGGERED flag
   2. If villain staggered, they recover instead of attacking
   3. Calculate attack/defense strengths
   4. Roll for result
   5. Apply damage to player

   NOTE: Original ZIL quirk - VILLAIN-BLOW doesn't check if the villain has a
   weapon before attacking. If a villain is disarmed (LOSE-WEAPON result from
   HERO-BLOW), they can still attack on the same turn with messages that
   reference their weapon (e.g., 'The troll swings his axe'). However, on the
   NEXT turn when the player attacks, HERO-BLOW checks FIND-WEAPON and
   auto-kills the unarmed villain ('The unarmed troll cannot defend himself:
   He dies.'). This creates a one-turn window where the messaging is
   inconsistent. Preserved for fidelity to original."
  [game-state villain-entry out?]
  (let [villain-id (:villain-id villain-entry)
        messages (:messages villain-entry)
        winner (:winner game-state)
        ;; Clear player's staggered flag
        gs (gs/unset-thing-flag game-state winner :staggered)]

    ;; Check if villain is staggered
    (if (gs/set-thing-flag? gs villain-id :staggered)
      ;; Villain recovers
      (-> gs
          (gs/unset-thing-flag villain-id :staggered)
          ;; Paragraph break for proper separation
          (utils/tell (str "\n\nThe " (get-thing-name gs villain-id) " slowly regains his feet.\n")))

      ;; Villain attacks
      (let [att (villain-strength gs villain-entry nil)  ; Villain's attack
            def (health/fight-strength gs)              ; Player's defense
            orig-def (health/fight-strength gs false)   ; Unadjusted base
            player-weapon (find-weapon gs winner)
            villain-name (get-thing-name gs villain-id)
            weapon-name (when player-weapon (get-thing-name gs player-weapon))]

        (cond
          ;; Player already dead/dying (def <= 0)
          (<= def 0)
          gs

          ;; Player defense is negative (already wounded badly)
          (neg? def)
          (let [message (select-combat-message messages killed "you" weapon-name)]
            (-> gs
                ;; Paragraph break for proper separation
                (utils/tell (str "\n\n" message "\n"))
                (winner-result 0 killed orig-def)))

          ;; Normal combat
          :else
          (let [[table _] (select-result-table def att)
                result (roll-combat-result table)
                ;; Apply OUT? modifier
                result (apply-out-modifier result (some? player-weapon) out?)
                ;; Get message
                message (select-combat-message messages result "you" weapon-name)
                ;; Calculate new defense
                new-def (cond
                          (or (= result missed) (= result hesitate))
                          def

                          (= result unconscious)
                          def  ; Just knocked out, not dead

                          (or (= result killed) (= result sitting-duck))
                          0

                          (= result light-wound)
                          (max 0 (- def 1))

                          (= result serious-wound)
                          (max 0 (- def 2))

                          :else def)]
            (-> gs
                ;; Paragraph break for proper separation
                (utils/tell (str "\n\n" message "\n"))
                ;; Set player staggered if result was stagger
                (cond-> (= result stagger)
                  (gs/set-thing-flag winner :staggered))
                ;; Handle weapon loss
                (cond-> (= result lose-weapon)
                  (-> (drop-weapon winner player-weapon)
                      ((fn [s]
                         (if-let [new-weapon (find-weapon s winner)]
                           (utils/tell s (str "\nFortunately, you still have a "
                                              (get-thing-name s new-weapon) "."))
                           s)))))
                ;; Apply damage
                (winner-result new-def result orig-def))))))))

;;; ---------------------------------------------------------------------------
;;; DO-FIGHT - Process all villain attacks
;;; ---------------------------------------------------------------------------
;;; ZIL: DO-FIGHT (1actions.zil line 3344)

(defn do-fight
  "Process attacks from all active villains.

   ZIL: DO-FIGHT (1actions.zil line 3344-3370)

   Iterates through villains with FIGHTBIT set and calls VILLAIN-BLOW.
   OUT? tracks when player is knocked down (reduced defense)."
  [game-state villains-map]
  (let [here (:here game-state)]
    ;; Return just the game state from the reduce
    (first (reduce
            (fn [[gs out?] [villain-id villain-entry]]
              (let [villain-loc (gs/get-thing-loc-id gs villain-id)
                    fighting? (gs/set-thing-flag? gs villain-id :fight)]
                (if (and (= villain-loc here) fighting?)
                  (let [result-gs (villain-blow gs villain-entry out?)
                        player-staggered? (gs/set-thing-flag? result-gs (:winner result-gs) :staggered)
                        new-out? (if player-staggered?
                                   (+ 1 (random/rand-int* 3))
                                   (when out? (max 0 (dec out?))))]
                    [result-gs new-out?])
                  [gs out?])))
            [game-state nil]
            villains-map))))

;;; ---------------------------------------------------------------------------
;;; AWAKEN - Wake up unconscious villain
;;; ---------------------------------------------------------------------------
;;; ZIL: AWAKEN (1actions.zil line 3856)

(defn awaken
  "Wake up an unconscious villain.

   ZIL: AWAKEN (1actions.zil line 3856-3860)

   If villain's strength is negative, negate it (make positive).
   Then call villain's F-CONSCIOUS action."
  [game-state villain-id]
  (let [strength (get-in game-state [:objects villain-id :strength] 0)]
    (if (neg? strength)
      (let [gs (assoc-in game-state [:objects villain-id :strength] (- strength))
            action-fn (get-in gs [:objects villain-id :action])]
        (if action-fn
          (action-fn gs :f-conscious)
          gs))
      game-state)))

;;; ---------------------------------------------------------------------------
;;; I-FIGHT - Combat daemon
;;; ---------------------------------------------------------------------------
;;; ZIL: I-FIGHT (1actions.zil line 3823)

(defn i-fight
  "Combat daemon - runs each turn to process villain attacks.

   ZIL: I-FIGHT (1actions.zil line 3823-3854)

   For each villain in the current room:
   - If invisible, skip
   - If unconscious, maybe wake up (probability increases each turn)
   - If has FIGHTBIT or wants to fight first, set fight? true
   - If player left room, clear fight flags

   Then if any villain wants to fight, call DO-FIGHT.

   Note: This requires villains to be registered in game-state :villains"
  [game-state villains-map]
  ;; Don't fight if player is dead
  (if (:dead game-state)
    game-state
    (let [here (:here game-state)
          winner (:winner game-state)
          ;; Process each villain
          [gs fight?] (reduce
                       (fn [[gs fight?] [villain-id villain-entry]]
                         (let [villain-loc (gs/get-thing-loc-id gs villain-id)
                               in-room? (= villain-loc here)
                               invisible? (gs/set-thing-flag? gs villain-id :invisible)
                               strength (get-in gs [:objects villain-id :strength] 0)]
                           (cond
                              ;; Villain not in room - clear fight flags if they were fighting
                             (not in-room?)
                             (let [was-fighting? (gs/set-thing-flag? gs villain-id :fight)]
                               (if was-fighting?
                                 [(-> gs
                                      (gs/unset-thing-flag winner :staggered)
                                      (gs/unset-thing-flag villain-id :staggered)
                                      (gs/unset-thing-flag villain-id :fight)
                                      (awaken villain-id))
                                  fight?]
                                 [gs fight?]))

                              ;; Skip invisible villains
                             invisible?
                             [gs fight?]

                              ;; Unconscious villain - maybe wake up
                             (neg? strength)
                             (let [wake-prob (get villain-entry :wake-prob 0)]
                               (if (and (pos? wake-prob) (< (random/rand-int* 100) wake-prob))
                                  ;; Wake up!
                                 [(-> gs
                                      (assoc-in [:villains villain-id :wake-prob] 0)
                                      (awaken villain-id))
                                  fight?]
                                  ;; Stay asleep, but increase wake probability
                                 [(update-in gs [:villains villain-id :wake-prob]
                                             (fn [p] (min 100 (+ (or p 0) 25))))
                                  fight?]))

                              ;; Active villain - check if wants to fight
                             :else
                             (let [has-fightbit? (gs/set-thing-flag? gs villain-id :fight)
                                    ;; Check F-FIRST? action - may return modified game-state with :fight flag set
                                   [new-gs wants-first?]
                                   (if has-fightbit?
                                     [gs false]
                                     (if-let [action (get-in gs [:objects villain-id :action])]
                                        ;; F-FIRST? should return modified game-state to initiate combat
                                       (let [result (action gs :f-first?)]
                                         (if (and result (not= result gs))
                                           [result true]  ; Use modified state
                                           [gs false]))
                                       [gs false]))]
                               [new-gs (or fight? has-fightbit? wants-first?)]))))
                       [game-state false]
                       villains-map)]
      ;; After checking all villains, fight if any want to
      (if fight?
        (do-fight gs villains-map)
        gs))))

;;; ---------------------------------------------------------------------------
;;; COMBAT DAEMON WRAPPER
;;; ---------------------------------------------------------------------------

(def villains-registry
  "Registry of all villains for combat.
   ZIL: VILLAINS global table (1actions.zil line 3814)

   This is the single source of truth for villain combat data.
   Used by both hero-blow (player attacks) and i-fight (villain counter-attacks)."
  {:troll {:villain-id :troll
           :best-weapon :sword
           :best-adv 1
           :wake-prob 0
           :messages troll-melee}
   :thief {:villain-id :thief
           :best-weapon :knife
           :best-adv 1
           :wake-prob 0
           :messages thief-melee}
   :cyclops {:villain-id :cyclops
             :best-weapon nil  ; Cyclops has no best-weapon weakness
             :best-adv 0
             :wake-prob 0
             :messages cyclops-melee}})

(defn combat-daemon
  "Combat daemon wrapper for the daemon system.
   Called each turn by the clocker to process villain attacks."
  [game-state]
  (i-fight game-state villains-registry))
