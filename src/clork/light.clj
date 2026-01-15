(ns clork.light
  "Light source daemons and helpers.

   Implements the lantern, candle, and match fuel/burn systems.

   ZIL Reference:
   - I-LANTERN (1actions.zil line 2328)
   - I-CANDLES (1actions.zil line 2334)
   - I-MATCH (1actions.zil line 2321)
   - LANTERN (1actions.zil line 2243)
   - CANDLES-FCN (1actions.zil line 2356)
   - MATCH-FUNCTION (1actions.zil line 2278)
   - LIGHT-INT helper (1actions.zil line 2341)"
  (:require [clork.game-state :as gs]
            [clork.utils :as utils]
            [clork.parser.state :as parser-state]
            [clork.daemon :as daemon]
            [clork.flags :as flags]
            [clork.verbs-look :as verbs-look]
            [clork.parser.validation :as validation]))

;;; ---------------------------------------------------------------------------
;;; LAMP TABLE - Fuel countdown stages
;;; ---------------------------------------------------------------------------
;;; ZIL: LAMP-TABLE in 1actions.zil
;;;
;;; <GLOBAL LAMP-TABLE
;;;   <TABLE (PURE)
;;;     100 "The lamp appears a bit dimmer."
;;;     70  "The lamp is definitely dimmer now."
;;;     15  "The lamp is nearly out."
;;;     0>>

(def lamp-stages
  "Lamp fuel countdown stages.
   Each entry: [turns-to-next-stage message]
   Total turns: 100 + 70 + 15 = 185 turns of light."
  [{:ticks 100 :message "The lamp appears a bit dimmer."}
   {:ticks 70  :message "The lamp is definitely dimmer now."}
   {:ticks 15  :message "The lamp is nearly out."}
   {:ticks 0   :message nil}])  ; 0 = burned out

;;; ---------------------------------------------------------------------------
;;; CANDLE TABLE - Burn countdown stages
;;; ---------------------------------------------------------------------------
;;; ZIL: CANDLE-TABLE in 1actions.zil
;;;
;;; <GLOBAL CANDLE-TABLE
;;;   <TABLE (PURE)
;;;     20 "The candles grow shorter."
;;;     10 "The candles are becoming quite short."
;;;     5  "The candles won't last long now."
;;;     0>>

(def candle-stages
  "Candle burn countdown stages.
   Each entry: [turns-to-next-stage message]
   Total turns: 20 + 10 + 5 = 35 turns of light."
  [{:ticks 20 :message "The candles grow shorter."}
   {:ticks 10 :message "The candles are becoming quite short."}
   {:ticks 5  :message "The candles won't last long now."}
   {:ticks 0  :message nil}])

;;; ---------------------------------------------------------------------------
;;; MATCH COUNT - Number of matches in matchbook
;;; ---------------------------------------------------------------------------
;;; ZIL: <GLOBAL MATCH-COUNT 6>

(def initial-match-count
  "Number of matches in a fresh matchbook."
  6)

;;; ---------------------------------------------------------------------------
;;; LIGHT-INT Helper - Common logic for light source countdown
;;; ---------------------------------------------------------------------------
;;; ZIL: LIGHT-INT (1actions.zil line 2341)
;;;
;;; <ROUTINE LIGHT-INT (OBJ TBL TICK)
;;;   <COND (<0? .TICK>
;;;     <FCLEAR .OBJ ,ONBIT>
;;;     <FSET .OBJ ,RMUNGBIT>)>
;;;   <COND (<OR <HELD? .OBJ> <IN? .OBJ ,HERE>>
;;;     <COND (<0? .TICK>
;;;       <TELL "You'd better have more light than from the " D .OBJ "." CR>)
;;;     (T
;;;       <TELL <GET .TBL 1> CR>)>)>>

(defn- light-int
  "Handle light source countdown message and burnout.

   ZIL: LIGHT-INT (1actions.zil line 2341)

   Parameters:
   - game-state: current game state
   - obj-id: the light source object (:brass-lantern, :candles)
   - stage: current countdown stage map {:ticks N :message S}

   If tick = 0 (burned out):
   - Clears :on flag, sets :burned-out flag
   - Prints 'You'd better have more light' message if object is visible

   Otherwise:
   - Prints the stage message if object is visible"
  [game-state obj-id stage]
  (let [ticks (:ticks stage)
        message (:message stage)
        obj-loc (gs/get-thing-loc-id game-state obj-id)
        here (:here game-state)
        winner (:winner game-state)
        visible? (or (= obj-loc winner) (= obj-loc here))
        obj-on? (gs/set-thing-flag? game-state obj-id :on)
        obj-desc (:desc (gs/get-thing game-state obj-id) "light")]
    (if (zero? ticks)
      ;; Burned out
      (let [gs (-> game-state
                   (gs/unset-thing-flag obj-id :on)
                   (gs/set-thing-flag obj-id :burned-out))]
        (if visible?
          (utils/tell gs (str "You'd better have more light than from the " obj-desc "."))
          gs))
      ;; Not burned out - print stage message only if object is ON
      ;; (candles blown out by wind should not print messages)
      (if (and visible? obj-on? message)
        (utils/tell game-state (str "\n" message))
        game-state))))

;;; ---------------------------------------------------------------------------
;;; I-LANTERN Daemon
;;; ---------------------------------------------------------------------------
;;; ZIL: I-LANTERN (1actions.zil line 2328)
;;;
;;; <ROUTINE I-LANTERN ("AUX" TICK (TBL <VALUE LAMP-TABLE>))
;;;   <ENABLE <QUEUE I-LANTERN <SET TICK <GET .TBL 0>>>>
;;;   <LIGHT-INT ,LAMP .TBL .TICK>
;;;   <COND (<NOT <0? .TICK>>
;;;     <SETG LAMP-TABLE <REST .TBL 4>>)>>

(defn i-lantern
  "Lantern fuel countdown daemon.

   ZIL: I-LANTERN (1actions.zil line 2328)

   Called when tick countdown reaches 0. Handles fuel stage transition:
   1. Gets current stage from lamp-stage-index
   2. Calls light-int to print message and handle burnout
   3. If not burned out, advances to next stage and queues next countdown"
  [game-state]
  (let [stage-idx (get game-state :lamp-stage-index 0)
        stage (get lamp-stages stage-idx)
        ticks (:ticks stage)]
    (if (nil? stage)
      ;; No more stages, lantern already burned out
      game-state
      ;; Process this stage
      (let [gs (light-int game-state :brass-lantern stage)]
        (if (zero? ticks)
          ;; Lantern burned out - disable daemon
          (daemon/disable gs :i-lantern)
          ;; Queue next stage
          (let [next-idx (inc stage-idx)
                next-stage (get lamp-stages next-idx)
                next-ticks (if next-stage (:ticks next-stage) 0)]
            (-> gs
                (assoc :lamp-stage-index next-idx)
                (daemon/queue :i-lantern next-ticks))))))))

;;; ---------------------------------------------------------------------------
;;; I-CANDLES Daemon
;;; ---------------------------------------------------------------------------
;;; ZIL: I-CANDLES (1actions.zil line 2334)

(defn i-candles
  "Candle burn countdown daemon.

   ZIL: I-CANDLES (1actions.zil line 2334)

   Similar to I-LANTERN but for candles, with shorter burn times."
  [game-state]
  (let [stage-idx (get game-state :candle-stage-index 0)
        stage (get candle-stages stage-idx)
        ticks (:ticks stage)]
    (if (nil? stage)
      ;; No more stages, candles already burned out
      game-state
      ;; Process this stage
      (let [gs (-> game-state
                   (gs/set-thing-flag :candles :touch)
                   (light-int :candles stage))]
        (if (zero? ticks)
          ;; Candles burned out - disable daemon
          (daemon/disable gs :i-candles)
          ;; Queue next stage
          (let [next-idx (inc stage-idx)
                next-stage (get candle-stages next-idx)
                next-ticks (if next-stage (:ticks next-stage) 0)]
            (-> gs
                (assoc :candle-stage-index next-idx)
                (daemon/queue :i-candles next-ticks))))))))

;;; ---------------------------------------------------------------------------
;;; I-MATCH Daemon
;;; ---------------------------------------------------------------------------
;;; ZIL: I-MATCH (1actions.zil line 2321)
;;;
;;; <ROUTINE I-MATCH ()
;;;   <TELL "The match has gone out." CR>
;;;   <FCLEAR ,MATCH ,FLAMEBIT>
;;;   <FCLEAR ,MATCH ,ONBIT>
;;;   <SETG LIT <LIT? ,HERE>>
;;;   <RTRUE>>

(defn i-match
  "Match burn daemon. Called after 2 turns when match goes out.

   ZIL: I-MATCH (1actions.zil line 2321)

   - Prints 'match has gone out' message
   - Clears :flame and :on flags
   - Recalculates room lighting"
  [game-state]
  (let [gs (-> game-state
               (utils/tell "\nThe match has gone out.")
               (gs/unset-thing-flag :matchbook :flame)
               (gs/unset-thing-flag :matchbook :on))]
    ;; Recalculate room lighting
    (assoc gs :lit (validation/lit? gs (:here gs)))))

;;; ---------------------------------------------------------------------------
;;; LANTERN Action Handler
;;; ---------------------------------------------------------------------------
;;; ZIL: LANTERN (1actions.zil line 2243)

(defn lantern-action
  "Lantern object action handler.

   ZIL: LANTERN (1actions.zil line 2243)

   Handles:
   - LAMP-ON: Enable daemon (if not burned out)
   - LAMP-OFF: Disable daemon
   - EXAMINE: Show status (burned out, on, off)
   - THROW: Smash lamp (creates broken-lamp)"
  [game-state]
  (let [prsa (parser-state/get-prsa game-state)
        burned-out? (gs/set-thing-flag? game-state :brass-lantern :burned-out)
        is-on? (gs/set-thing-flag? game-state :brass-lantern :on)]
    (cond
      ;; LAMP-ON
      (= prsa :lamp-on)
      (cond
        burned-out?
        (utils/tell game-state "A burned-out lamp won't light.")

        is-on?
        (utils/tell game-state "It is already on.")

        :else
        ;; Turn on and enable daemon
        ;; ZIL: <COND (<NOT ,LIT> <SETG LIT <LIT? ,HERE>> <CRLF> <V-LOOK>)>)>
        (let [stage-idx (get game-state :lamp-stage-index 0)
              stage (get lamp-stages stage-idx)
              ticks (if stage (:ticks stage) 0)
              was-dark? (not (:lit game-state))
              gs (-> game-state
                     (gs/set-thing-flag :brass-lantern :on)
                     (daemon/queue :i-lantern ticks)
                     (utils/tell "The brass lantern is now on."))]
          ;; Update :lit flag and show room if it was dark
          (if was-dark?
            (-> gs
                (assoc :lit (validation/lit? gs (:here gs)))
                utils/crlf
                verbs-look/v-look)
            (assoc gs :lit true))))

      ;; LAMP-OFF
      ;; ZIL: <COND (,LIT <SETG LIT <LIT? ,HERE>>)>
      ;;      <TELL "The " D ,PRSO " is now off." CR>
      ;;      <COND (<NOT ,LIT> <TELL "It is now pitch black." CR>)>
      (= prsa :lamp-off)
      (cond
        burned-out?
        (utils/tell game-state "The lamp has already burned out.")

        (not is-on?)
        (utils/tell game-state "It is already off.")

        :else
        (let [gs (-> game-state
                     (gs/unset-thing-flag :brass-lantern :on)
                     (daemon/disable :i-lantern))
              still-lit? (validation/lit? gs (:here gs))
              gs (-> gs
                     (assoc :lit still-lit?)
                     (utils/tell "The brass lantern is now off."))]
          (if (not still-lit?)
            (utils/tell gs "It is now pitch black.")
            gs)))

      ;; EXAMINE
      (= prsa :examine)
      (cond
        burned-out?
        (utils/tell game-state "The lamp has burned out.")

        is-on?
        (utils/tell game-state "The lamp is on.")

        :else
        (utils/tell game-state "The lamp is turned off."))

      ;; Default - not handled
      :else nil)))

;;; ---------------------------------------------------------------------------
;;; MATCH-FUNCTION Action Handler
;;; ---------------------------------------------------------------------------
;;; ZIL: MATCH-FUNCTION (1actions.zil line 2278)

(def drafty-rooms
  "Rooms where matches go out instantly due to drafts."
  #{:lower-shaft :timber-room})

(defn match-action
  "Matchbook/match object action handler.

   ZIL: MATCH-FUNCTION (1actions.zil line 2278)

   Handles:
   - LAMP-ON/BURN: Light a match (decrements count, queues I-MATCH)
   - COUNT: Show matches remaining
   - EXAMINE: Show matches remaining"
  [game-state]
  (let [prsa (parser-state/get-prsa game-state)
        prso (parser-state/get-prso game-state)
        here (:here game-state)
        match-count (get game-state :match-count initial-match-count)
        is-on? (gs/set-thing-flag? game-state :matchbook :on)]
    (cond
      ;; LAMP-ON or BURN on match
      (and (#{:lamp-on :burn} prsa) (= prso :matchbook))
      (cond
        ;; Already burning
        is-on?
        (utils/tell game-state "The match is already lit.")

        ;; No matches left
        (<= match-count 0)
        (utils/tell game-state "I'm afraid that you have run out of matches.")

        ;; Drafty room - instant extinguish
        (contains? drafty-rooms here)
        (-> game-state
            (assoc :match-count (dec match-count))
            (utils/tell "This room is drafty, and the match goes out instantly."))

        ;; Light the match
        :else
        (let [gs (-> game-state
                     (assoc :match-count (dec match-count))
                     (gs/set-thing-flag :matchbook :flame)
                     (gs/set-thing-flag :matchbook :on)
                     (daemon/queue :i-match 2)
                     (utils/tell "One of the matches starts to burn."))]
          ;; If room was dark, look around
          (if (not (:lit game-state))
            (-> gs
                (assoc :lit true)
                verbs-look/v-look)
            gs)))

      ;; COUNT verb
      (= prsa :count)
      (let [remaining (get game-state :match-count initial-match-count)]
        (utils/tell game-state
                    (case remaining
                      0 "There are no matches left."
                      1 "There is one match left."
                      (str "There are " remaining " matches left."))))

      ;; EXAMINE
      (= prsa :examine)
      (let [remaining (get game-state :match-count initial-match-count)]
        (if is-on?
          (utils/tell game-state "The match is burning.")
          (utils/tell game-state
                      (case remaining
                        0 "The matchbook is empty."
                        1 "There is one match left in the matchbook."
                        (str "There are " remaining " matches in the matchbook.")))))

      ;; LAMP-OFF
      (= prsa :lamp-off)
      (if is-on?
        (-> game-state
            (gs/unset-thing-flag :matchbook :flame)
            (gs/unset-thing-flag :matchbook :on)
            (daemon/disable :i-match)
            (utils/tell "The match is out."))
        (utils/tell game-state "The match isn't lit."))

      ;; Default - not handled
      :else nil)))

;;; ---------------------------------------------------------------------------
;;; CANDLES-FCN Action Handler
;;; ---------------------------------------------------------------------------
;;; ZIL: CANDLES-FCN (1actions.zil line 2356)

(defn candles-action
  "Candles object action handler.

   ZIL: CANDLES-FCN (1actions.zil line 2356)

   Handles:
   - LAMP-ON: Light candles (requires lit match or torch)
   - LAMP-OFF: Extinguish candles
   - EXAMINE: Show status"
  [game-state]
  (let [prsa (parser-state/get-prsa game-state)
        prsi (parser-state/get-prsi game-state)
        burned-out? (gs/set-thing-flag? game-state :candles :burned-out)
        is-on? (gs/set-thing-flag? game-state :candles :on)
        match-lit? (gs/set-thing-flag? game-state :matchbook :on)
        torch-held? (= (gs/get-thing-loc-id game-state :ivory-torch) (:winner game-state))]
    (cond
      ;; LAMP-ON
      (= prsa :lamp-on)
      (cond
        burned-out?
        (utils/tell game-state "The candles have burned out.")

        is-on?
        (utils/tell game-state "The candles are already lit.")

        ;; Light with torch specifically - destroys candles!
        ;; ZIL: Only triggers when PRSI is the torch, not just when holding it
        (= prsi :ivory-torch)
        (-> game-state
            (gs/move-object :candles :limbo :torch-vaporize)  ; Remove candles
            (utils/tell "The heat from the torch is so intense that the candles are vaporized."))

        ;; Light with match
        (or (= prsi :matchbook) match-lit?)
        (let [stage-idx (get game-state :candle-stage-index 0)
              stage (get candle-stages stage-idx)
              ticks (if stage (:ticks stage) 0)]
          (-> game-state
              (gs/set-thing-flag :candles :on)
              (daemon/queue :i-candles ticks)
              (cond-> (not prsi)
                (utils/tell "(with the match)"))
              (utils/tell "The candles are lit.")))

        ;; No fire source
        :else
        (utils/tell game-state "You need a flame to light the candles."))

      ;; LAMP-OFF
      (= prsa :lamp-off)
      (cond
        burned-out?
        (utils/tell game-state "The candles have already burned out.")

        (not is-on?)
        (utils/tell game-state "The candles are not lit.")

        :else
        (-> game-state
            (gs/unset-thing-flag :candles :on)
            (daemon/disable :i-candles)
            (utils/tell "The candles are out.")))

      ;; EXAMINE
      (= prsa :examine)
      (cond
        burned-out?
        (utils/tell game-state "The candles have burned down to stubs.")

        is-on?
        (utils/tell game-state "The candles are lit.")

        :else
        (utils/tell game-state "The candles are not lit."))

      ;; Default - not handled
      :else nil)))
