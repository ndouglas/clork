(ns clork.verb-defs
  "Verb definitions - single source of truth for vocabulary, syntax, and handlers."
  (:require [clork.utils :as utils]
            [clork.game-state :as game-state]
            [clork.verbs-meta :as verbs-meta]
            [clork.verbs-health :as verbs-health]
            [clork.verbs-inventory :as verbs-inv]
            [clork.verbs-containers :as verbs-containers]
            [clork.verbs-movement :as verbs-movement]
            [clork.verbs-look :as verbs-look]
            [clork.verbs-light :as verbs-light]
            [clork.verbs-put :as verbs-put]
            [clork.verbs-combat :as verbs-combat]
            [clork.verbs-food :as verbs-food]
            [clork.verbs-misc :as verbs-misc]
            [clork.cyclops :as cyclops]
            [clork.loud-room :as loud-room]
            [clork.daemon :as daemon]
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
                :handler verbs-meta/v-verbose}

   :brief      {:words   ["brief"]
                :syntax  {:num-objects 0}
                :handler verbs-meta/v-brief}

   :super-brief {:words   ["superbrief" "super-brief"]
                 :syntax  {:num-objects 0}
                 :handler verbs-meta/v-super-brief}

   :version    {:words   ["version"]
                :syntax  {:num-objects 0}
                :handler verbs-meta/v-version}

   :diagnose   {:words   ["diagnose"]
                :syntax  {:num-objects 0}
                :handler verbs-health/v-diagnose}

   :score      {:words   ["score"]
                :syntax  {:num-objects 0}
                :handler verbs-health/v-score}

   :quit       {:words   ["quit" "q"]
                :syntax  {:num-objects 0}
                :handler verbs-health/v-quit}

   ;; ZIL: <SYNTAX RESTART = V-RESTART>
   :restart    {:words   ["restart"]
                :syntax  {:num-objects 0}
                :handler verbs-health/v-restart}

   ;; ZIL: <SYNTAX SAVE = V-SAVE>
   :save       {:words   ["save"]
                :syntax  {:num-objects 0}
                :handler verbs-health/v-save}

   ;; ZIL: <SYNTAX RESTORE = V-RESTORE>
   :restore    {:words   ["restore"]
                :syntax  {:num-objects 0}
                :handler verbs-health/v-restore}

   ;; ZIL: <SYNTAX SCRIPT = V-SCRIPT>
   :script     {:words   ["script"]
                :syntax  {:num-objects 0}
                :handler verbs-health/v-script}

   ;; ZIL: <SYNTAX UNSCRIPT = V-UNSCRIPT>
   :unscript   {:words   ["unscript"]
                :syntax  {:num-objects 0}
                :handler verbs-health/v-unscript}

   ;; ZIL: <SYNTAX $VERIFY = V-VERIFY>
   ;; Note: In ZIL, $VERIFY used $ prefix for debugging commands.
   ;; We support both "verify" and "$verify" for compatibility.
   :verify     {:words   ["verify" "$verify"]
                :syntax  {:num-objects 0}
                :handler verbs-health/v-verify}

   ;; ZIL: <SYNTAX WAIT = V-WAIT>
   ;;      <SYNONYM WAIT Z>
   ;; Wait passes time, running daemons for several turns.
   :wait       {:words   ["wait" "z"]
                :syntax  {:num-objects 0}
                :handler verbs-meta/v-wait}

   ;; === Observation Verbs ===
   ;; ZIL: <SYNTAX LOOK = V-LOOK>
   ;;      <SYNTAX LOOK ON OBJECT = V-LOOK-ON>
   ;;      <SYNTAX LOOK UNDER OBJECT = V-LOOK-UNDER>
   ;;      <SYNTAX LOOK BEHIND OBJECT = V-LOOK-BEHIND>
   ;;      <SYNTAX LOOK IN OBJECT = V-LOOK-INSIDE>
   :look       {:words   ["look" "l"]
                :syntax  [;; LOOK (bare) - describe room
                          {:num-objects 0}

                          ;; LOOK UNDER OBJECT
                          {:num-objects 1
                           :prep1 :under
                           :loc1 #{:held :carried :in-room :on-ground}
                           :action :look-under}

                          ;; LOOK BEHIND OBJECT
                          {:num-objects 1
                           :prep1 :behind
                           :loc1 #{:held :carried :in-room :on-ground}
                           :action :look-behind}

                          ;; LOOK ON OBJECT
                          {:num-objects 1
                           :prep1 :on
                           :loc1 #{:held :carried :in-room :on-ground}
                           :action :look-on}

                          ;; LOOK IN OBJECT
                          {:num-objects 1
                           :prep1 :in
                           :loc1 #{:held :carried :in-room :on-ground}
                           :action :look-inside}]
                :handler verbs-look/v-look}

   ;; Handler for LOOK UNDER (routed via :look syntax)
   :look-under {:words   []
                :syntax  {:num-objects 1
                          :loc1 #{:held :carried :in-room :on-ground}}
                :handler verbs-containers/v-look-under}

   ;; Handler for LOOK BEHIND (routed via :look syntax)
   :look-behind {:words   []
                 :syntax  {:num-objects 1
                           :loc1 #{:held :carried :in-room :on-ground}}
                 :handler verbs-containers/v-look-behind}

   ;; Handler for LOOK ON (routed via :look syntax)
   :look-on    {:words   []
                :syntax  {:num-objects 1
                          :loc1 #{:held :carried :in-room :on-ground}}
                :handler verbs-containers/v-look-on}

   :inventory  {:words   ["inventory" "i"]
                :syntax  {:num-objects 0}
                :handler verbs-meta/v-inventory}

   ;; === Manipulation Verbs ===
   ;; ZIL: TAKE has multiple syntaxes:
   ;;   <SYNTAX TAKE OBJECT (ON-GROUND IN-ROOM MANY) = V-TAKE>
   ;;   <SYNTAX TAKE OBJECT FROM OBJECT (ON-GROUND) = V-TAKE>
   ;;   <SYNTAX TAKE OBJECT OFF OBJECT (ON-GROUND) = V-TAKE>
   ;;   <SYNTAX TAKE OBJECT OUT OF OBJECT (ON-GROUND) = V-TAKE>
   :take       {:words   ["take" "get" "hold" "carry" "remove" "grab" "catch"]
                :syntax  [;; Basic: TAKE OBJECT
                          {:num-objects 1
                           :loc1 #{:in-room :on-ground :many}}

                          ;; TAKE OBJECT FROM OBJECT - take from container
                          {:num-objects 2
                           :prep2 :from
                           :loc1 #{:carried :in-room :many}
                           :loc2 #{:on-ground}}

                          ;; TAKE OBJECT OFF OBJECT - take off surface
                          {:num-objects 2
                           :prep2 :off
                           :loc1 #{:carried :in-room :many}
                           :loc2 #{:on-ground}}]
                :handler verbs-inv/v-take}

   :read       {:words   ["read" "skim"]
                :syntax  {:num-objects 1
                          :loc1 #{:held :carried :in-room :on-ground :take}}
                :handler verbs-inv/v-read}

   :drop       {:words   ["drop" "throw" "discard"]
                :syntax  {:num-objects 1
                          :loc1 #{:held :many :have}}
                :handler verbs-inv/v-drop}

   ;; ZIL: <SYNTAX PUT OBJECT (HELD MANY HAVE) IN OBJECT = V-PUT PRE-PUT>
   ;;      <SYNTAX PUT OBJECT (HELD MANY HAVE) ON OBJECT = V-PUT-ON PRE-PUT>
   ;;      <SYNTAX PUT OBJECT (HELD HAVE) UNDER OBJECT = V-PUT-UNDER>
   ;;      <SYNTAX PUT OBJECT (HELD MANY HAVE) BEHIND OBJECT = V-PUT-BEHIND>
   :put        {:words   ["put" "place" "insert" "stuff"]
                :syntax  [;; PUT OBJECT IN OBJECT
                          {:num-objects 2
                           :prep2 :in
                           :loc1 #{:held :carried :many :have}
                           :loc2 #{:held :carried :in-room :on-ground}
                           :action :put}

                          ;; PUT OBJECT ON OBJECT
                          {:num-objects 2
                           :prep2 :on
                           :loc1 #{:held :carried :many :have}
                           :loc2 #{:held :carried :in-room :on-ground}
                           :action :put-on}

                          ;; PUT OBJECT UNDER OBJECT
                          {:num-objects 2
                           :prep2 :under
                           :loc1 #{:held :have}
                           :loc2 #{:held :carried :in-room :on-ground}
                           :action :put-under}

                          ;; PUT OBJECT BEHIND OBJECT
                          {:num-objects 2
                           :prep2 :behind
                           :loc1 #{:held :carried :many :have}
                           :loc2 #{:held :carried :in-room :on-ground}
                           :action :put-behind}]
                :handler verbs-put/v-put}

   ;; Action handlers for PUT variants (called by dispatcher based on :action)
   :put-on     {:words   []  ; No direct words, routed via :action
                :syntax  {:num-objects 0}  ; Not used directly
                :handler verbs-put/v-put-on}

   :put-under  {:words   []
                :syntax  {:num-objects 0}
                :handler verbs-put/v-put-under}

   :put-behind {:words   []
                :syntax  {:num-objects 0}
                :handler verbs-put/v-put-behind}

   ;; ZIL: <SYNTAX OPEN OBJECT (FIND DOORBIT) (HELD CARRIED ON-GROUND IN-ROOM) = V-OPEN>
   ;;      <SYNTAX OPEN OBJECT (FIND DOORBIT) (HELD CARRIED ON-GROUND IN-ROOM) WITH
   ;;              OBJECT (FIND TOOLBIT) (ON-GROUND IN-ROOM HELD CARRIED HAVE) = V-OPEN>
   :open       {:words   ["open"]
                :syntax  [{;; Basic: OPEN OBJECT
                           :num-objects 1
                           :loc1 #{:held :in-room :on-ground :carried}}
                          ;; OPEN OBJECT WITH OBJECT
                          {:num-objects 2
                           :prep2 :with
                           :loc1 #{:held :in-room :on-ground :carried}
                           :loc2 #{:held :carried :in-room :on-ground :have}
                           :gwim2 :tool}]  ; FIND TOOLBIT - find tools
                :handler verbs-containers/v-open}

   ;; ZIL: <SYNTAX CLOSE OBJECT (FIND DOORBIT) (HELD CARRIED ON-GROUND IN-ROOM) = V-CLOSE>
   :close      {:words   ["close" "shut"]
                :syntax  {:num-objects 1
                          :loc1 #{:held :in-room :on-ground :carried}}
                :handler verbs-containers/v-close}

   :examine    {:words   ["examine" "x" "describe" "what" "whats"]
                :syntax  {:num-objects 1
                          :loc1 #{:held :carried :in-room :on-ground :many}}
                :handler verbs-containers/v-examine}

   :look-inside {:words   ["search"]
                 :syntax  {:num-objects 1
                           :loc1 #{:held :carried :in-room :on-ground}}
                 :handler verbs-containers/v-look-inside}

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
                           :action :through}

                          ;; WALK UP OBJECT - go up chimney, climb up stairs
                          {:num-objects 1
                           :prep1 :up
                           :loc1 #{:in-room :on-ground}
                           :action :climb-up}

                          ;; WALK DOWN OBJECT - go down stairs
                          {:num-objects 1
                           :prep1 :down
                           :loc1 #{:in-room :on-ground}
                           :action :climb-down}]
                :handler verbs-movement/v-walk}

   ;; ZIL: <SYNTAX ENTER = V-ENTER> (bare enter goes to :in direction)
   ;;      <SYNTAX ENTER OBJECT = V-THROUGH>
   ;; Go through a door, window, or other passageway
   :through    {:words   ["enter" "through"]
                :syntax  [;; ENTER (bare) - walk in :in direction
                          {:num-objects 0
                           :action :enter}

                          ;; ENTER OBJECT / THROUGH OBJECT - go through passageway
                          {:num-objects 1
                           :loc1 #{:in-room :on-ground}}]
                :handler verbs-movement/v-through}

   ;; Handler for bare enter - reached via :through syntax with 0 objects
   :enter      {:words   []  ; No direct words - reached via :through syntax
                :syntax  {:num-objects 0}
                :handler verbs-movement/v-enter}

   ;; Handler for walk-around - used when WALK AROUND OBJECT is parsed
   ;; The verb words aren't used directly since this routes through :walk
   :walk-around {:words   []  ; No direct words - reached via :walk syntax
                 :syntax  {:num-objects 1
                           :loc1 #{:in-room :on-ground}}
                 :handler verbs-movement/v-walk-around}

   ;; ZIL: <SYNTAX BACK = V-BACK>
   ;; Note: Original Zork I doesn't track previous room - just shows error
   :back        {:words   ["back" "return"]
                 :syntax  {:num-objects 0}
                 :handler verbs-movement/v-back}

   ;; === Manipulation Verbs (continued) ===
   ;; ZIL: <SYNTAX MOVE OBJECT (ON-GROUND IN-ROOM) = V-MOVE PRE-MOVE>
   ;;      <SYNTAX PULL OBJECT (ON-GROUND IN-ROOM) = V-MOVE PRE-MOVE>
   ;;      <SYNTAX ROLL OBJECT (ON-GROUND IN-ROOM) = V-MOVE PRE-MOVE>
   :move       {:words   ["move" "pull" "roll" "push" "slide" "shift"]
                :syntax  {:num-objects 1
                          :loc1 #{:in-room :on-ground}}
                :handler verbs-movement/v-move}

   ;; === Climb Verbs ===
   ;; ZIL: <SYNTAX CLIMB UP OBJECT (FIND CLIMBBIT) (ON-GROUND IN-ROOM) = V-CLIMB-UP>
   ;;      <SYNTAX CLIMB DOWN OBJECT (FIND CLIMBBIT) (ON-GROUND IN-ROOM) = V-CLIMB-DOWN>
   ;;      <SYNTAX CLIMB OBJECT (FIND CLIMBBIT) (ON-GROUND IN-ROOM) = V-CLIMB-FOO>
   ;;      <SYNTAX CLIMB ON OBJECT (FIND VEHBIT) (ON-GROUND IN-ROOM) = V-CLIMB-ON>
   ;;      <SYNONYM CLIMB SIT>
   :climb-up   {:words   []  ; Reached via :climb syntax with "up" prep
                :syntax  {:num-objects 1
                          :loc1 #{:in-room :on-ground}}
                :handler verbs-movement/v-climb-up}

   :climb-down {:words   []  ; Reached via :climb syntax with "down" prep
                :syntax  {:num-objects 1
                          :loc1 #{:in-room :on-ground}}
                :handler verbs-movement/v-climb-down}

   :climb-foo  {:words   []  ; Reached via :climb syntax without direction
                :syntax  {:num-objects 1
                          :loc1 #{:in-room :on-ground}}
                :handler verbs-movement/v-climb-foo}

   :climb-on   {:words   []  ; Reached via :climb syntax with "on" prep
                :syntax  {:num-objects 1
                          :loc1 #{:in-room :on-ground}}
                :handler verbs-movement/v-climb-on}

   ;; Main climb verb with multiple syntaxes
   ;; ZIL: <SYNTAX CLIMB UP OBJECT (FIND RMUNGBIT) = V-CLIMB-UP>
   ;;      <SYNTAX CLIMB UP OBJECT (FIND CLIMBBIT) (ON-GROUND IN-ROOM) = V-CLIMB-UP>
   ;;      <SYNTAX CLIMB DOWN OBJECT (FIND RMUNGBIT) = V-CLIMB-DOWN>
   ;;      <SYNTAX CLIMB DOWN OBJECT (FIND CLIMBBIT) (ON-GROUND IN-ROOM) = V-CLIMB-DOWN>
   ;;      <SYNTAX CLIMB OBJECT (FIND CLIMBBIT) (ON-GROUND IN-ROOM) = V-CLIMB-FOO>
   ;;      <SYNTAX CLIMB IN OBJECT (FIND VEHBIT) (ON-GROUND IN-ROOM) = V-BOARD PRE-BOARD>
   ;;      <SYNTAX CLIMB ON OBJECT (FIND VEHBIT) (ON-GROUND IN-ROOM) = V-CLIMB-ON>
   ;;      <SYNTAX CLIMB WITH OBJECT = V-THROUGH>
   ;;      <SYNONYM CLIMB SIT>
   ;; Note: RMUNGBIT allows finding room as pseudo-object for bare "climb up/down".
   ;; Without RMUNGBIT, users should use "up"/"down" or "climb <object>".
   :climb      {:words   ["climb" "sit"]
                :syntax  [;; CLIMB UP OBJECT (FIND RMUNGBIT) - climb up (no object, defaults to ROOMS)
                          ;; ZIL: <SYNTAX CLIMB UP OBJECT (FIND RMUNGBIT) = V-CLIMB-UP>
                          {:num-objects 1
                           :prep1 :up
                           :gwim1 :room  ; RMUNGBIT - returns :rooms pseudo-object
                           :action :climb-up}

                          ;; CLIMB UP OBJECT (FIND CLIMBBIT) - climb up tree
                          ;; ZIL: <SYNTAX CLIMB UP OBJECT (FIND CLIMBBIT) (ON-GROUND IN-ROOM) = V-CLIMB-UP>
                          {:num-objects 1
                           :prep1 :up
                           :loc1 #{:in-room :on-ground}
                           :action :climb-up}

                          ;; CLIMB DOWN OBJECT (FIND RMUNGBIT) - climb down (no object, defaults to ROOMS)
                          ;; ZIL: <SYNTAX CLIMB DOWN OBJECT (FIND RMUNGBIT) = V-CLIMB-DOWN>
                          {:num-objects 1
                           :prep1 :down
                           :gwim1 :room  ; RMUNGBIT
                           :action :climb-down}

                          ;; CLIMB DOWN OBJECT (FIND CLIMBBIT) - climb down tree
                          ;; ZIL: <SYNTAX CLIMB DOWN OBJECT (FIND CLIMBBIT) (ON-GROUND IN-ROOM) = V-CLIMB-DOWN>
                          {:num-objects 1
                           :prep1 :down
                           :loc1 #{:in-room :on-ground}
                           :action :climb-down}

                          ;; CLIMB OBJECT (FIND CLIMBBIT) - climb tree (no direction, defaults to up)
                          ;; ZIL: <SYNTAX CLIMB OBJECT (FIND CLIMBBIT) (ON-GROUND IN-ROOM) = V-CLIMB-FOO>
                          {:num-objects 1
                           :loc1 #{:in-room :on-ground}
                           :action :climb-foo}

                          ;; CLIMB IN OBJECT - board vehicle (V-BOARD)
                          ;; ZIL: <SYNTAX CLIMB IN OBJECT (FIND VEHBIT) (ON-GROUND IN-ROOM) = V-BOARD PRE-BOARD>
                          ;; TODO: Route to :board when implemented
                          {:num-objects 1
                           :prep1 :in
                           :loc1 #{:in-room :on-ground}
                           :action :through}

                          ;; CLIMB ON OBJECT - climb on boat
                          ;; ZIL: <SYNTAX CLIMB ON OBJECT (FIND VEHBIT) (ON-GROUND IN-ROOM) = V-CLIMB-ON>
                          {:num-objects 1
                           :prep1 :on
                           :loc1 #{:in-room :on-ground}
                           :action :climb-on}

                          ;; CLIMB WITH OBJECT - go through (V-THROUGH)
                          ;; ZIL: <SYNTAX CLIMB WITH OBJECT = V-THROUGH>
                          {:num-objects 1
                           :prep1 :with
                           :loc1 #{:in-room :on-ground}
                           :action :through}]
                :handler verbs-movement/v-climb-foo}

   ;; === Light Source Verbs ===
   ;; ZIL: <SYNTAX TURN ON OBJECT (FIND LIGHTBIT) (HELD CARRIED ON-GROUND IN-ROOM) = V-LAMP-ON>
   ;;      <SYNTAX TURN OFF OBJECT (FIND ONBIT) (HELD CARRIED ON-GROUND IN-ROOM TAKE HAVE) = V-LAMP-OFF>
   ;;      <SYNTAX TURN OBJECT (FIND TURNBIT) (HELD CARRIED ON-GROUND IN-ROOM) WITH OBJECT = V-TURN>
   ;; The "turn" verb routes to different handlers based on preposition (on/off/with)
   ;; Note: :gwim1 :light allows finding light sources even in darkness (FIND LIGHTBIT)
   :turn       {:words   ["turn" "switch"]
                :syntax  [;; TURN ON OBJECT (FIND LIGHTBIT) - turn on lamp
                          {:num-objects 1
                           :prep1 :on
                           :gwim1 :light  ; FIND LIGHTBIT - can find in dark
                           :loc1 #{:held :carried :in-room :on-ground}
                           :action :lamp-on}

                          ;; TURN OFF OBJECT (FIND ONBIT) - turn off lamp
                          {:num-objects 1
                           :prep1 :off
                           :gwim1 :on  ; FIND ONBIT - can find lit objects in dark
                           :loc1 #{:held :carried :in-room :on-ground}
                           :action :lamp-off}

                          ;; TURN OBJECT WITH OBJECT - turn bolt with wrench
                          ;; ZIL: <SYNTAX TURN OBJECT (FIND TURNBIT) (HELD CARRIED ON-GROUND IN-ROOM) WITH OBJECT = V-TURN>
                          {:num-objects 2
                           :prep2 :with
                           :loc1 #{:held :carried :in-room :on-ground}
                           :loc2 #{:held :carried}}

                          ;; TURN OBJECT - turn bolt (no "with" preposition)
                          ;; ZIL: <SYNTAX TURN OBJECT (FIND TURNBIT) (HELD CARRIED ON-GROUND IN-ROOM) = V-TURN>
                          {:num-objects 1
                           :loc1 #{:held :carried :in-room :on-ground}}]
                :handler verbs-movement/v-turn}

   ;; Additional words for lamp-on
   ;; ZIL: <SYNTAX LIGHT OBJECT (FIND LIGHTBIT) (HELD CARRIED ON-GROUND IN-ROOM TAKE HAVE) = V-LAMP-ON>
   ;;      <SYNTAX ACTIVATE OBJECT (FIND LIGHTBIT) (HELD CARRIED ON-GROUND IN-ROOM) = V-LAMP-ON>
   :lamp-on    {:words   ["light" "activate"]
                :syntax  [{;; LIGHT OBJECT (FIND LIGHTBIT) - light lamp
                           :num-objects 1
                           :gwim1 :light  ; FIND LIGHTBIT - can find in dark
                           :loc1 #{:held :carried :in-room :on-ground}}

                          ;; LIGHT OBJECT WITH OBJECT - light candle with match
                          {:num-objects 2
                           :prep2 :with
                           :gwim1 :light
                           :loc1 #{:held :carried :in-room :on-ground}
                           :loc2 #{:held :carried}}]
                :handler verbs-light/v-lamp-on}

   ;; ZIL: <SYNTAX EXTINGUISH OBJECT (FIND ONBIT) (HELD CARRIED ON-GROUND IN-ROOM) = V-LAMP-OFF>
   ;;      <SYNTAX DOUSE OBJECT (FIND ONBIT) (HELD CARRIED ON-GROUND IN-ROOM) = V-LAMP-OFF>
   :lamp-off   {:words   ["extinguish" "douse"]
                :syntax  {:num-objects 1
                          :gwim1 :on  ; FIND ONBIT - can find lit objects in dark
                          :loc1 #{:held :carried :in-room :on-ground}}
                :handler verbs-light/v-lamp-off}

   ;; === Combat Verbs ===
   ;; ZIL: <SYNTAX ATTACK OBJECT (FIND ACTORBIT) (ON-GROUND IN-ROOM)
   ;;              WITH OBJECT (FIND WEAPONBIT) (HELD CARRIED HAVE) = V-ATTACK>
   ;;      <SYNONYM ATTACK FIGHT HURT INJURE HIT>
   :attack     {:words   ["attack" "fight" "hurt" "injure" "hit"]
                :syntax  {:num-objects 2
                          :prep2 :with
                          :gwim1 :actor  ; FIND ACTORBIT - find actors
                          :gwim2 :weapon ; FIND WEAPONBIT - find weapons
                          :loc1 #{:in-room :on-ground}
                          :loc2 #{:held :carried :have}}
                :handler verbs-combat/v-attack}

   ;; ZIL: <SYNTAX KILL OBJECT (FIND ACTORBIT) (ON-GROUND IN-ROOM)
   ;;              WITH OBJECT (FIND WEAPONBIT) (HELD CARRIED HAVE) = V-ATTACK>
   ;;      <SYNONYM KILL MURDER SLAY DISPATCH>
   :kill       {:words   ["kill" "murder" "slay" "dispatch"]
                :syntax  {:num-objects 2
                          :prep2 :with
                          :gwim1 :actor
                          :gwim2 :weapon
                          :loc1 #{:in-room :on-ground}
                          :loc2 #{:held :carried :have}}
                :handler verbs-combat/v-attack}

   ;; ZIL: <SYNTAX STAB OBJECT (FIND ACTORBIT) (ON-GROUND IN-ROOM) = V-STAB>
   ;;      (Stab without specifying weapon)
   :stab       {:words   ["stab"]
                :syntax  [{;; STAB OBJECT - auto-find weapon
                           :num-objects 1
                           :gwim1 :actor
                           :loc1 #{:in-room :on-ground}}
                          ;; STAB OBJECT WITH WEAPON
                          {:num-objects 2
                           :prep2 :with
                           :gwim1 :actor
                           :gwim2 :weapon
                           :loc1 #{:in-room :on-ground}
                           :loc2 #{:held :carried :have}}]
                :handler verbs-combat/v-stab}

   ;; === Special/Easter Egg Verbs ===
   ;; ZIL: <SYNTAX ODYSSEUS = V-ODYSSEUS>
   ;;      <SYNONYM ODYSSEUS ULYSSES>
   ;; Saying "Odysseus" or "Ulysses" scares the cyclops (his father's nemesis)
   :odysseus   {:words   ["odysseus" "ulysses"]
                :syntax  {:num-objects 0}
                :handler cyclops/v-odysseus}

   ;; ZIL: <SYNTAX ECHO = V-ECHO>
   ;; Saying "echo" in the Loud Room solves the puzzle
   :echo       {:words   ["echo"]
                :syntax  {:num-objects 0}
                :handler loud-room/v-echo}

   ;; === Food/Drink Verbs ===
   ;; ZIL: <SYNTAX EAT OBJECT (FIND FOODBIT) (HELD CARRIED ON-GROUND IN-ROOM TAKE) = V-EAT>
   :eat        {:words   ["eat" "consume" "taste" "bite" "munch"]
                :syntax  {:num-objects 1
                          :gwim1 :food  ; FIND FOODBIT
                          :loc1 #{:held :carried :in-room :on-ground :take}}
                :handler verbs-food/v-eat}

   ;; ZIL: <SYNTAX DRINK OBJECT (FIND DRINKBIT) (HELD CARRIED ON-GROUND IN-ROOM) = V-DRINK>
   ;;      <SYNTAX DRINK FROM OBJECT (HELD CARRIED) = V-DRINK-FROM>
   ;;      <SYNONYM DRINK IMBIBE SWALLOW>
   :drink      {:words   ["drink" "imbibe" "swallow" "sip" "quaff"]
                :syntax  [{;; DRINK OBJECT (FIND DRINKBIT)
                           :num-objects 1
                           :gwim1 :drink  ; FIND DRINKBIT
                           :loc1 #{:held :carried :in-room :on-ground}}

                          ;; DRINK FROM OBJECT
                          {:num-objects 1
                           :prep1 :from
                           :loc1 #{:held :carried}
                           :action :drink-from}]
                :handler verbs-food/v-drink}

   ;; Handler for DRINK FROM (routed via :drink syntax)
   :drink-from {:words   []
                :syntax  {:num-objects 1
                          :loc1 #{:held :carried}}
                :handler verbs-food/v-drink-from}

   ;; === Communication Verbs ===
   ;; ZIL: <SYNTAX GIVE OBJECT (MANY HELD HAVE) TO OBJECT (FIND ACTORBIT) (ON-GROUND) = V-GIVE PRE-GIVE>
   ;;      <SYNTAX GIVE OBJECT (FIND ACTORBIT) (ON-GROUND) OBJECT (MANY HELD HAVE) = V-SGIVE PRE-SGIVE>
   :give       {:words   ["give" "hand" "offer" "feed"]
                :syntax  {:num-objects 2
                          :prep2 :to
                          :loc1 #{:held :have :many}
                          :loc2 #{:in-room :on-ground}
                          :gwim2 :actor}  ; FIND ACTORBIT
                :handler verbs-food/v-give}

   ;; === Lock Verbs ===
   ;; ZIL: <SYNTAX LOCK OBJECT (ON-GROUND IN-ROOM) WITH OBJECT (FIND TOOLBIT) (HELD CARRIED ON-GROUND IN-ROOM TAKE) = V-LOCK>
   :lock       {:words   ["lock"]
                :syntax  {:num-objects 2
                          :prep2 :with
                          :loc1 #{:in-room :on-ground}
                          :loc2 #{:held :carried :in-room :on-ground :take}
                          :gwim2 :tool}  ; FIND TOOLBIT
                :handler verbs-food/v-lock}

   ;; ZIL: <SYNTAX UNLOCK OBJECT (ON-GROUND IN-ROOM) WITH OBJECT (FIND TOOLBIT) (HELD CARRIED ON-GROUND IN-ROOM TAKE) = V-UNLOCK>
   :unlock     {:words   ["unlock"]
                :syntax  {:num-objects 2
                          :prep2 :with
                          :loc1 #{:in-room :on-ground}
                          :loc2 #{:held :carried :in-room :on-ground :take}
                          :gwim2 :tool}  ; FIND TOOLBIT
                :handler verbs-food/v-unlock}

   ;; === Simple/Message Verbs ===
   ;; ZIL: <SYNTAX JUMP = V-LEAP>
   ;;      <SYNTAX JUMP OVER OBJECT = V-LEAP>
   :jump       {:words   ["jump" "leap" "hop"]
                :syntax  [{:num-objects 0}
                          {:num-objects 1 :prep1 :over :loc1 #{:in-room :on-ground}}
                          {:num-objects 1 :prep1 :across :loc1 #{:in-room :on-ground}}
                          {:num-objects 1 :prep1 :in :loc1 #{:in-room :on-ground}}
                          {:num-objects 1 :prep1 :from :loc1 #{:in-room :on-ground}}
                          {:num-objects 1 :prep1 :off :loc1 #{:in-room :on-ground}}]
                :handler verbs-misc/v-jump}

   ;; ZIL: <SYNTAX SWIM = V-SWIM>
   :swim       {:words   ["swim" "wade"]
                :syntax  [{:num-objects 0}
                          {:num-objects 1 :prep1 :in :loc1 #{:in-room :on-ground}}
                          {:num-objects 1 :prep1 :across :loc1 #{:in-room :on-ground}}]
                :handler verbs-misc/v-swim}

   ;; ZIL: <SYNTAX LISTEN TO OBJECT = V-LISTEN>
   :listen     {:words   ["listen"]
                :syntax  [{:num-objects 0}
                          {:num-objects 1 :prep1 :to :loc1 #{:in-room :on-ground :held}}]
                :handler verbs-misc/v-listen}

   ;; ZIL: <SYNTAX SMELL OBJECT = V-SMELL>
   :smell      {:words   ["smell" "sniff"]
                :syntax  [{:num-objects 0}
                          {:num-objects 1 :loc1 #{:in-room :on-ground :held}}]
                :handler verbs-misc/v-smell}

   ;; ZIL: <SYNTAX KICK OBJECT = V-KICK>
   :kick       {:words   ["kick"]
                :syntax  {:num-objects 1 :loc1 #{:in-room :on-ground}}
                :handler verbs-misc/v-kick}

   ;; ZIL: <SYNTAX KISS OBJECT = V-KISS>
   :kiss       {:words   ["kiss" "hug" "embrace"]
                :syntax  {:num-objects 1 :loc1 #{:in-room :on-ground}}
                :handler verbs-misc/v-kiss}

   ;; ZIL: <SYNTAX KNOCK ON OBJECT = V-KNOCK>
   :knock      {:words   ["knock"]
                :syntax  {:num-objects 1 :prep1 :on :loc1 #{:in-room :on-ground}}
                :handler verbs-misc/v-knock}

   ;; ZIL: <SYNTAX PRAY = V-PRAY>
   :pray       {:words   ["pray"]
                :syntax  {:num-objects 0}
                :handler verbs-misc/v-pray}

   ;; ZIL: <SYNTAX STAND = V-STAND>
   ;;      <SYNTAX STAND UP = V-STAND>
   :stand      {:words   ["stand"]
                :syntax  [{:num-objects 0}
                          {:num-objects 0 :prep1 :up}]
                :handler verbs-misc/v-stand}

   ;; ZIL: <SYNTAX FIND OBJECT = V-FIND>
   ;;      <SYNTAX WHERE IS OBJECT = V-FIND>
   :find       {:words   ["find" "where" "locate"]
                :syntax  {:num-objects 1 :loc1 #{:held :in-room :on-ground :carried}}
                :handler verbs-misc/v-find}

   ;; ZIL: <SYNTAX COUNT OBJECT = V-COUNT>
   :count      {:words   ["count"]
                :syntax  [{:num-objects 0}
                          {:num-objects 1 :loc1 #{:held :in-room :on-ground}}]
                :handler verbs-misc/v-count}

   ;; ZIL: <SYNTAX RING OBJECT = V-RING>
   :ring       {:words   ["ring"]
                :syntax  {:num-objects 1 :loc1 #{:held :in-room :on-ground}}
                :handler verbs-misc/v-ring}

   ;; === Object Manipulation Verbs ===
   ;; ZIL: <SYNTAX WAVE OBJECT (HELD HAVE) = V-WAVE>
   :wave       {:words   ["wave"]
                :syntax  {:num-objects 1 :loc1 #{:held :have}}
                :handler verbs-misc/v-wave}

   ;; ZIL: <SYNTAX RUB OBJECT = V-RUB>
   ;;      <SYNTAX TOUCH OBJECT = V-RUB>
   :rub        {:words   ["rub" "touch" "feel" "fondle" "pet"]
                :syntax  {:num-objects 1 :loc1 #{:in-room :on-ground :held}}
                :handler verbs-misc/v-rub}

   ;; ZIL: <SYNTAX RAISE OBJECT = V-RAISE>
   ;;      <SYNTAX LIFT OBJECT = V-RAISE>
   :raise      {:words   ["raise" "lift"]
                :syntax  {:num-objects 1 :loc1 #{:in-room :on-ground :held}}
                :handler verbs-misc/v-raise}

   ;; ZIL: <SYNTAX LOWER OBJECT = V-LOWER>
   :lower      {:words   ["lower"]
                :syntax  {:num-objects 1 :loc1 #{:in-room :on-ground :held}}
                :handler verbs-misc/v-lower}

   ;; ZIL: <SYNTAX SHAKE OBJECT = V-SHAKE>
   :shake      {:words   ["shake"]
                :syntax  {:num-objects 1 :loc1 #{:held :in-room :on-ground}}
                :handler verbs-misc/v-shake}

   ;; ZIL: <SYNTAX TIE OBJECT TO OBJECT = V-TIE>
   :tie        {:words   ["tie" "fasten" "attach"]
                :syntax  {:num-objects 2 :prep2 :to
                          :loc1 #{:held}
                          :loc2 #{:in-room :on-ground}}
                :handler verbs-misc/v-tie}

   ;; ZIL: <SYNTAX UNTIE OBJECT = V-UNTIE>
   :untie      {:words   ["untie" "unfasten" "detach"]
                :syntax  {:num-objects 1 :loc1 #{:in-room :on-ground :held}}
                :handler verbs-misc/v-untie}

   ;; ZIL: <SYNTAX SWING OBJECT (HELD) = V-SWING>
   ;;      <SYNTAX SWING OBJECT (HELD) AT OBJECT = V-SWING>
   :swing      {:words   ["swing"]
                :syntax  [{:num-objects 1 :loc1 #{:held}}
                          {:num-objects 2 :prep2 :at :loc1 #{:held} :loc2 #{:in-room}}]
                :handler verbs-misc/v-swing}

   ;; === Communication Verbs ===
   ;; ZIL: <SYNTAX HELLO = V-HELLO>
   ;;      <SYNTAX HELLO OBJECT = V-HELLO>
   :hello      {:words   ["hello" "hi" "greetings"]
                :syntax  [{:num-objects 0}
                          {:num-objects 1 :loc1 #{:in-room :on-ground}}]
                :handler verbs-misc/v-hello}

   ;; ZIL: <SYNTAX YELL = V-YELL>
   ;;      <SYNTAX SCREAM = V-YELL>
   :yell       {:words   ["yell" "scream" "shout"]
                :syntax  {:num-objects 0}
                :handler verbs-misc/v-yell}

   ;; ZIL: <SYNTAX SAY STRING = V-SAY>
   :say        {:words   ["say" "speak"]
                :syntax  {:num-objects 0}  ;; String handling TBD
                :handler verbs-misc/v-say}

   ;; === Easter Egg / Meta Verbs ===
   ;; ZIL: <SYNTAX CURSE OBJECT = V-CURSES>
   ;;      <SYNTAX DAMN OBJECT = V-CURSES>
   :curse      {:words   ["curse" "damn" "shit" "fuck" "crap" "hell" "darn"]
                :syntax  [{:num-objects 0}
                          {:num-objects 1 :loc1 #{:in-room :on-ground}}]
                :handler verbs-misc/v-curses}

   ;; ZIL: <SYNTAX WIN = V-WIN>
   :win        {:words   ["win"]
                :syntax  {:num-objects 0}
                :handler verbs-misc/v-win}

   ;; ZIL: <SYNTAX ZORK = V-ZORK>
   :zork       {:words   ["zork"]
                :syntax  {:num-objects 0}
                :handler verbs-misc/v-zork}

   ;; ZIL: <SYNTAX FROBOZZ = V-FROBOZZ>
   :frobozz    {:words   ["frobozz"]
                :syntax  {:num-objects 0}
                :handler verbs-misc/v-frobozz}

   ;; ZIL: <SYNTAX XYZZY = V-ADVENT>
   ;;      <SYNTAX PLUGH = V-ADVENT>
   :xyzzy      {:words   ["xyzzy" "plugh"]
                :syntax  {:num-objects 0}
                :handler verbs-misc/v-xyzzy}

   ;; ZIL: <SYNTAX TREASURE = V-TREASURE>
   ;;      <SYNTAX TEMPLE = V-TREASURE>
   :treasure   {:words   ["treasure" "temple"]
                :syntax  {:num-objects 0}
                :handler verbs-misc/v-treasure}

   ;; === Tool Verbs (stubs) ===
   ;; ZIL: <SYNTAX CUT OBJECT WITH OBJECT (FIND WEAPONBIT) = V-CUT>
   :cut        {:words   ["cut" "slice" "chop"]
                :syntax  {:num-objects 2 :prep2 :with
                          :loc1 #{:in-room :on-ground :held}
                          :loc2 #{:held}}
                :handler verbs-misc/v-cut}

   ;; ZIL: <SYNTAX DIG OBJECT WITH OBJECT = V-DIG>
   ;;      <SYNTAX DIG IN OBJECT WITH OBJECT = V-DIG>
   :dig        {:words   ["dig"]
                :syntax  [{:num-objects 1 :loc1 #{:in-room :on-ground}}
                          {:num-objects 2 :prep2 :with :loc1 #{:in-room :on-ground} :loc2 #{:held}}]
                :handler verbs-misc/v-dig}

   ;; ZIL: <SYNTAX BURN OBJECT (ON-GROUND IN-ROOM) WITH OBJECT = V-BURN>
   :burn       {:words   ["burn" "ignite" "light"]
                :syntax  {:num-objects 2 :prep2 :with
                          :loc1 #{:in-room :on-ground}
                          :loc2 #{:held}}
                :handler verbs-misc/v-burn}

   ;; ZIL: <SYNTAX FILL OBJECT WITH OBJECT = V-FILL>
   :fill       {:words   ["fill"]
                :syntax  {:num-objects 2 :prep2 :with
                          :loc1 #{:held :in-room}
                          :loc2 #{:in-room :on-ground}}
                :handler verbs-misc/v-fill}

   ;; ZIL: <SYNTAX POUR OBJECT = V-POUR>
   ;;      <SYNTAX POUR OBJECT ON OBJECT = V-POUR-ON>
   :pour       {:words   ["pour" "spill"]
                :syntax  [{:num-objects 1 :loc1 #{:held}}
                          {:num-objects 2 :prep2 :on :loc1 #{:held} :loc2 #{:in-room :on-ground}}]
                :handler verbs-misc/v-pour}

   ;; ZIL: <SYNTAX INFLATE OBJECT (ON-GROUND IN-ROOM) WITH OBJECT = V-INFLATE>
   :inflate    {:words   ["inflate"]
                :syntax  [{:num-objects 1 :loc1 #{:in-room :on-ground}}
                          {:num-objects 2 :prep2 :with :loc1 #{:in-room :on-ground} :loc2 #{:held}}]
                :handler verbs-misc/v-inflate}

   ;; ZIL: <SYNTAX DEFLATE OBJECT = V-DEFLATE>
   :deflate    {:words   ["deflate"]
                :syntax  {:num-objects 1 :loc1 #{:in-room :on-ground :held}}
                :handler verbs-misc/v-deflate}

   ;; ZIL: <SYNTAX CROSS OBJECT = V-CROSS>
   :cross      {:words   ["cross"]
                :syntax  {:num-objects 1 :loc1 #{:in-room :on-ground}}
                :handler verbs-misc/v-cross}

   ;; ZIL: <SYNTAX LAUNCH OBJECT = V-LAUNCH>
   :launch     {:words   ["launch"]
                :syntax  {:num-objects 1 :loc1 #{:in-room :on-ground}}
                :handler verbs-misc/v-launch}

   ;; ZIL: <SYNTAX BOARD OBJECT = V-BOARD PRE-BOARD>
   :board      {:words   ["board"]
                :syntax  {:num-objects 1 :loc1 #{:in-room :on-ground}}
                :handler verbs-misc/v-board}

   ;; ZIL: <SYNTAX DISEMBARK = V-DISEMBARK>
   ;;      <SYNTAX DISEMBARK OBJECT = V-DISEMBARK>
   :disembark  {:words   ["disembark" "debark"]
                :syntax  [{:num-objects 0}
                          {:num-objects 1 :loc1 #{:in-room :on-ground}}]
                :handler verbs-misc/v-disembark}

   ;; ZIL: <SYNTAX WEAR OBJECT (HELD) = V-WEAR>
   :wear       {:words   ["wear" "don"]
                :syntax  {:num-objects 1 :loc1 #{:held}}
                :handler verbs-misc/v-wear}

   ;;; ------------------------------------------------------------------------
   ;;; NPC INTERACTION VERBS
   ;;; ------------------------------------------------------------------------

   ;; ZIL: <SYNTAX WAKE OBJECT (FIND ACTORBIT) = V-ALARM>
   :alarm      {:words   ["wake" "awaken"]
                :syntax  [{:num-objects 0}
                          {:num-objects 1 :loc1 #{:in-room :on-ground}}]
                :handler verbs-misc/v-alarm}

   ;; ZIL: <SYNTAX TELL OBJECT (FIND ACTORBIT) = V-TELL>
   ;;      <SYNTAX TALK TO OBJECT = V-TELL>
   :tell       {:words   ["tell" "ask"]
                :syntax  {:num-objects 1 :loc1 #{:in-room :on-ground}}
                :handler verbs-misc/v-tell}

   ;; ZIL: <SYNTAX ANSWER = V-ANSWER>
   :answer     {:words   ["answer"]
                :syntax  {:num-objects 0}
                :handler verbs-misc/v-answer}

   ;; ZIL: <SYNTAX ANSWER OBJECT = V-REPLY>
   :reply      {:words   ["reply"]
                :syntax  {:num-objects 1 :loc1 #{:in-room :on-ground}}
                :handler verbs-misc/v-reply}

   ;; ZIL: <SYNTAX COMMAND OBJECT (FIND ACTORBIT) = V-COMMAND>
   :command    {:words   ["command" "order"]
                :syntax  {:num-objects 1 :loc1 #{:in-room :on-ground}}
                :handler verbs-misc/v-command}

   ;; ZIL: <SYNTAX FOLLOW = V-FOLLOW>
   :follow     {:words   ["follow"]
                :syntax  [{:num-objects 0}
                          {:num-objects 1 :loc1 #{:in-room :on-ground}}]
                :handler verbs-misc/v-follow}

   ;; ZIL: <SYNTAX SEND FOR OBJECT = V-SEND>
   :send       {:words   ["send"]
                :syntax  {:num-objects 1 :prep1 :for :loc1 #{:in-room :on-ground}}
                :handler verbs-misc/v-send}

   ;;; ------------------------------------------------------------------------
   ;;; COMBAT/VIOLENCE VERBS
   ;;; ------------------------------------------------------------------------

   ;; ZIL: <SYNTAX BLAST = V-BLAST>
   :blast      {:words   ["blast"]
                :syntax  [{:num-objects 0}
                          {:num-objects 1 :loc1 #{:in-room :on-ground}}]
                :handler verbs-misc/v-blast}

   ;; ZIL: <SYNTAX DESTROY OBJECT = V-MUNG>
   :mung       {:words   ["destroy" "mung" "damage"]
                :syntax  {:num-objects 1 :loc1 #{:in-room :on-ground :held}}
                :handler verbs-misc/v-mung}

   ;; ZIL: <SYNTAX STRIKE OBJECT = V-STRIKE>
   :strike     {:words   ["strike"]
                :syntax  {:num-objects 1 :loc1 #{:in-room :on-ground :held}}
                :handler verbs-misc/v-strike}

   ;; ZIL: <SYNTAX THROW OBJECT AT OBJECT = V-THROW>
   :throw      {:words   ["throw" "hurl" "toss"]
                :syntax  [{:num-objects 1 :loc1 #{:held}}
                          {:num-objects 2 :prep1 :at :loc1 #{:held} :loc2 #{:in-room :on-ground}}
                          {:num-objects 2 :prep1 :to :loc1 #{:held} :loc2 #{:in-room :on-ground}}]
                :handler verbs-misc/v-throw}

   ;; ZIL: <SYNTAX THROW OBJECT OFF OBJECT = V-THROW-OFF>
   :throw-off  {:words   []  ; Reached via throw with "off" prep
                :syntax  {:num-objects 2 :prep1 :off :loc1 #{:held} :loc2 #{:in-room :on-ground}}
                :handler verbs-misc/v-throw-off}

   ;; ZIL: <SYNTAX TIE UP OBJECT WITH OBJECT = V-TIE-UP>
   :tie-up     {:words   []  ; Reached via "tie up X with Y"
                :syntax  {:num-objects 2}
                :handler verbs-misc/v-tie-up}

   ;;; ------------------------------------------------------------------------
   ;;; MAGIC VERBS (Zork II/III stubs)
   ;;; ------------------------------------------------------------------------

   ;; ZIL: <SYNTAX INCANT = V-INCANT>
   :incant     {:words   ["incant" "cast" "spell"]
                :syntax  {:num-objects 0}
                :handler verbs-misc/v-incant}

   ;; ZIL: <SYNTAX ENCHANT OBJECT = V-ENCHANT>
   :enchant    {:words   ["enchant"]
                :syntax  {:num-objects 1 :loc1 #{:in-room :on-ground}}
                :handler verbs-misc/v-enchant}

   ;; ZIL: <SYNTAX DISENCHANT OBJECT = V-DISENCHANT>
   :disenchant {:words   ["disenchant"]
                :syntax  {:num-objects 1 :loc1 #{:in-room :on-ground}}
                :handler verbs-misc/v-disenchant}

   ;; ZIL: <SYNTAX EXORCISE OBJECT = V-EXORCISE>
   :exorcise   {:words   ["exorcise" "banish"]
                :syntax  [{:num-objects 0}
                          {:num-objects 1 :loc1 #{:in-room :on-ground}}]
                :handler verbs-misc/v-exorcise}

   ;;; ------------------------------------------------------------------------
   ;;; OBJECT ACTION VERBS
   ;;; ------------------------------------------------------------------------

   ;; ZIL: <SYNTAX BRUSH OBJECT = V-BRUSH>
   :brush      {:words   ["brush"]
                :syntax  [{:num-objects 1 :loc1 #{:held :in-room :on-ground}}
                          {:num-objects 2 :prep1 :with :loc1 #{:held :in-room :on-ground} :loc2 #{:held}}]
                :handler verbs-misc/v-brush}

   ;; ZIL: <SYNTAX SQUEEZE OBJECT = V-SQUEEZE>
   :squeeze    {:words   ["squeeze" "squish"]
                :syntax  {:num-objects 1 :loc1 #{:held :in-room :on-ground}}
                :handler verbs-misc/v-squeeze}

   ;; ZIL: <SYNTAX SPIN OBJECT = V-SPIN>
   :spin       {:words   ["spin" "rotate"]
                :syntax  {:num-objects 1 :loc1 #{:held :in-room :on-ground}}
                :handler verbs-misc/v-spin}

   ;; ZIL: <SYNTAX WIND OBJECT = V-WIND>
   :wind       {:words   ["wind"]
                :syntax  {:num-objects 1 :loc1 #{:held :in-room :on-ground}}
                :handler verbs-misc/v-wind}

   ;; ZIL: <SYNTAX PICK OBJECT = V-PICK>
   :pick       {:words   ["pick"]
                :syntax  [{:num-objects 1 :loc1 #{:in-room :on-ground}}
                          {:num-objects 2 :prep1 :with :loc1 #{:in-room :on-ground} :loc2 #{:held}}]
                :handler verbs-misc/v-pick}

   ;; ZIL: <SYNTAX LUBRICATE OBJECT WITH OBJECT = V-OIL>
   :oil        {:words   ["oil" "lubricate" "grease"]
                :syntax  {:num-objects 2 :prep1 :with :loc1 #{:in-room :on-ground} :loc2 #{:held}}
                :handler verbs-misc/v-oil}

   ;; ZIL: <SYNTAX MELT OBJECT = V-MELT>
   :melt       {:words   ["melt"]
                :syntax  {:num-objects 1 :loc1 #{:held :in-room :on-ground}}
                :handler verbs-misc/v-melt}

   ;; ZIL: <SYNTAX SPRAY OBJECT ON OBJECT = V-SPRAY>
   :spray      {:words   ["spray"]
                :syntax  [{:num-objects 1 :loc1 #{:held}}
                          {:num-objects 2 :prep1 :on :loc1 #{:held} :loc2 #{:in-room :on-ground}}]
                :handler verbs-misc/v-spray}

   ;;; ------------------------------------------------------------------------
   ;;; CONTAINER/POSITION VERBS
   ;;; ------------------------------------------------------------------------

   ;; ZIL: <SYNTAX SEARCH OBJECT = V-SEARCH>
   ;; Note: "search" word is assigned to :look-inside for container searching
   :search     {:words   []
                :syntax  [{:num-objects 0}
                          {:num-objects 1 :loc1 #{:in-room :on-ground}}]
                :handler verbs-misc/v-search}

   ;; ZIL: <SYNTAX POUR OBJECT ON OBJECT = V-POUR-ON>
   :pour-on    {:words   []  ; Reached via "pour X on Y"
                :syntax  {:num-objects 2 :prep1 :on :loc1 #{:held} :loc2 #{:in-room :on-ground}}
                :handler verbs-misc/v-pour-on}

   ;; ZIL: <SYNTAX PLUG OBJECT WITH OBJECT = V-PLUG>
   :plug       {:words   ["plug" "patch" "repair"]
                :syntax  {:num-objects 2 :prep1 :with :loc1 #{:in-room :on-ground} :loc2 #{:held}}
                :handler verbs-misc/v-plug}

   ;; ZIL: <SYNTAX PUMP UP OBJECT = V-PUMP>
   :pump       {:words   ["pump"]
                :syntax  [{:num-objects 1 :loc1 #{:in-room :on-ground}}
                          {:num-objects 2 :prep1 :with :loc1 #{:in-room :on-ground} :loc2 #{:held}}]
                :handler verbs-misc/v-pump}

   ;;; ------------------------------------------------------------------------
   ;;; MISCELLANEOUS VERBS
   ;;; ------------------------------------------------------------------------

   ;; ZIL: <SYNTAX BLOW IN OBJECT = V-BREATHE>
   :breathe    {:words   ["breathe" "blow"]
                :syntax  {:num-objects 1 :prep1 :in :loc1 #{:held :in-room :on-ground}}
                :handler verbs-misc/v-breathe}

   ;; ZIL: <SYNTAX CHOMP = V-CHOMP>
   :chomp      {:words   ["chomp" "bite" "chew"]
                :syntax  [{:num-objects 0}
                          {:num-objects 1 :loc1 #{:held :in-room :on-ground}}]
                :handler verbs-misc/v-chomp}

   ;; ZIL: <SYNTAX LEAN ON OBJECT = V-LEAN-ON>
   :lean-on    {:words   ["lean"]
                :syntax  {:num-objects 1 :prep1 :on :loc1 #{:in-room :on-ground}}
                :handler verbs-misc/v-lean-on}

   ;; ZIL: <SYNTAX MAKE OBJECT = V-MAKE>
   :make       {:words   ["make" "build" "construct"]
                :syntax  {:num-objects 1}
                :handler verbs-misc/v-make}

   ;; ZIL: <SYNTAX PLAY OBJECT = V-PLAY>
   :play       {:words   ["play"]
                :syntax  {:num-objects 1 :loc1 #{:held :in-room :on-ground}}
                :handler verbs-misc/v-play}

   ;; ZIL: <SYNTAX STAY = V-STAY>
   :stay       {:words   ["stay"]
                :syntax  {:num-objects 0}
                :handler verbs-misc/v-stay}

   ;; ZIL: <SYNTAX WISH = V-WISH>
   :wish       {:words   ["wish"]
                :syntax  {:num-objects 0}
                :handler verbs-misc/v-wish}

   ;; ZIL: <SYNTAX HATCH OBJECT = V-HATCH>
   :hatch      {:words   ["hatch"]
                :syntax  {:num-objects 1 :loc1 #{:held :in-room :on-ground}}
                :handler verbs-misc/v-hatch}

   ;; ZIL: <SYNTAX MUMBLE = V-MUMBLE>
   :mumble     {:words   ["mumble" "mutter"]
                :syntax  {:num-objects 0}
                :handler verbs-misc/v-mumble}

   ;; ZIL: <SYNTAX REPENT = V-REPENT>
   :repent     {:words   ["repent"]
                :syntax  {:num-objects 0}
                :handler verbs-misc/v-repent}

   ;;; ------------------------------------------------------------------------
   ;;; META/DEBUG VERBS
   ;;; ------------------------------------------------------------------------

   ;; ZIL: <SYNTAX BUG = V-BUG>
   :bug        {:words   ["bug"]
                :syntax  {:num-objects 0}
                :handler verbs-misc/v-bug}})

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

(defn- merge-vocab-entries
  "Merge two vocabulary entries, combining parts-of-speech and keeping all values."
  [existing new]
  (-> existing
      (update :parts-of-speech into (:parts-of-speech new))
      (cond-> (:verb-value new) (assoc :verb-value (:verb-value new)))
      (cond-> (:dir-value new) (assoc :dir-value (:dir-value new)))
      (cond-> (:prep-value new) (assoc :prep-value (:prep-value new)))
      (cond-> (:object-value new) (assoc :object-value (:object-value new)))
      (cond-> (:adj-value new) (assoc :adj-value (:adj-value new)))))

(def ^:dynamic *verb-vocabulary*
  "Vocabulary entries for verbs. Merged with object/direction/preposition vocabulary."
  (merge-with merge-vocab-entries
              (build-vocabulary verb-definitions)
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
  ;; Call room's M-BEG action first (ZIL: room actions get M-BEG at start of turn)
  ;; If room fully handles the command, it sets :command-handled to prevent verb
  (let [game-state (daemon/call-room-m-beg game-state)]
    (if (:command-handled game-state)
      ;; Room already handled the command - just clear the flag and return
      (dissoc game-state :command-handled)
      ;; Normal verb processing
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
            gs))))))
