(ns clork.verbs-light
  "Light source handlers: lamp-on, lamp-off.

   ZIL Reference: V-LAMP-ON, V-LAMP-OFF in gverbs.zil."
  (:require [clork.utils :as utils]
            [clork.game-state :as gs]
            [clork.parser.state :as parser-state]
            [clork.verbs-look :as verbs-look]))

;;; ---------------------------------------------------------------------------
;;; LAMP-ON COMMAND
;;; ---------------------------------------------------------------------------
;;; ZIL: V-LAMP-ON in gverbs.zil (lines 802-817)

(defn v-lamp-on
  "Turn on a light source.

   ZIL: V-LAMP-ON in gverbs.zil:
   <ROUTINE V-LAMP-ON ()
     <COND (<FSET? ,PRSO ,LIGHTBIT>
            <COND (<FSET? ,PRSO ,ONBIT>
                   <TELL \"It is already on.\" CR>)
                  (T
                   <FSET ,PRSO ,ONBIT>
                   <TELL \"The \" D ,PRSO \" is now on.\" CR>
                   <COND (<NOT ,LIT>
                          <SETG LIT <LIT? ,HERE>>
                          <CRLF>
                          <V-LOOK>)>)>)
           (<FSET? ,PRSO ,BURNBIT>
            <TELL \"If you wish to burn the \" D ,PRSO \", you should say so.\" CR>)
           (T
            <TELL \"You can't turn that on.\" CR>)>>"
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        obj (gs/get-thing game-state prso)
        desc (:desc obj)
        action-fn (:action obj)
        has-light? (gs/set-thing-flag? game-state prso :light)
        has-burn? (gs/set-thing-flag? game-state prso :burn)
        is-on? (gs/set-thing-flag? game-state prso :on)]
    ;; First try the object's action handler (e.g., lantern, match, candles)
    (if-let [result (when action-fn (action-fn game-state))]
      result
      ;; Object didn't handle it - default behavior
      (cond
        ;; Object has :light flag (can be turned on/off)
        has-light?
        (cond
          ;; Already on
          is-on?
          (utils/tell game-state "It is already on.")

          ;; Turn it on
          :else
          (let [was-dark? (not (:lit game-state))
                ;; Set the :on flag
                gs (gs/set-thing-flag game-state prso :on)
                ;; Update room lit status
                gs (assoc gs :lit true)
                ;; Tell the player
                gs (utils/tell gs (str "The " desc " is now on."))]
            ;; If room was dark, show the room description
            (if was-dark?
              (-> gs
                  utils/crlf
                  verbs-look/v-look)
              gs)))

        ;; Object has :burn flag (needs fire to light)
        has-burn?
        (utils/tell game-state (str "If you wish to burn the " desc ", you should say so."))

        ;; Can't turn this on
        :else
        (utils/tell game-state "You can't turn that on.")))))

;;; ---------------------------------------------------------------------------
;;; LAMP-OFF COMMAND
;;; ---------------------------------------------------------------------------
;;; ZIL: V-LAMP-OFF in gverbs.zil (lines 787-800)

(defn- room-still-lit?
  "Check if the room is still lit after turning off a light source.
   Looks for other light sources that are on."
  [game-state]
  (let [here (:here game-state)
        winner (:winner game-state)
        contents (gs/get-contents game-state winner)]
    (or
     ;; Room is naturally lit
     (gs/set-thing-flag? game-state here :lit)
     ;; Player has another light source that's on
     (some (fn [obj-id]
             (and (gs/set-thing-flag? game-state obj-id :light)
                  (gs/set-thing-flag? game-state obj-id :on)))
           contents))))

(defn v-lamp-off
  "Turn off a light source.

   ZIL: V-LAMP-OFF in gverbs.zil:
   <ROUTINE V-LAMP-OFF ()
     <COND (<FSET? ,PRSO ,LIGHTBIT>
            <COND (<NOT <FSET? ,PRSO ,ONBIT>>
                   <TELL \"It is already off.\" CR>)
                  (T
                   <FCLEAR ,PRSO ,ONBIT>
                   <COND (,LIT
                          <SETG LIT <LIT? ,HERE>>)>
                   <TELL \"The \" D ,PRSO \" is now off.\" CR>
                   <COND (<NOT ,LIT>
                          <TELL \"It is now pitch black.\" CR>)>)>)
           (T
            <TELL \"You can't turn that off.\" CR>)>
     <RTRUE>>"
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        obj (gs/get-thing game-state prso)
        desc (:desc obj)
        action-fn (:action obj)
        has-light? (gs/set-thing-flag? game-state prso :light)
        is-on? (gs/set-thing-flag? game-state prso :on)]
    ;; First try the object's action handler (e.g., lantern, match, candles)
    (if-let [result (when action-fn (action-fn game-state))]
      result
      ;; Object didn't handle it - default behavior
      (cond
        ;; Object has :light flag (can be turned on/off)
        has-light?
        (cond
          ;; Already off
          (not is-on?)
          (utils/tell game-state "It is already off.")

          ;; Turn it off
          :else
          (let [;; Clear the :on flag
                gs (gs/unset-thing-flag game-state prso :on)
                ;; Check if room is still lit
                still-lit? (room-still-lit? gs)
                gs (assoc gs :lit still-lit?)
                ;; Tell the player
                gs (utils/tell gs (str "The " desc " is now off."))]
            ;; If now dark, warn the player
            (if (not still-lit?)
              (utils/tell gs "It is now pitch black.")
              gs)))

        ;; Can't turn this off
        :else
        (utils/tell game-state "You can't turn that off.")))))
