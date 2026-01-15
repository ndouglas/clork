(ns clork.verbs-containers
  "Container and inspection handlers: open, examine, look-inside.

   ZIL Reference: V-OPEN, V-EXAMINE, V-LOOK-INSIDE in gverbs.zil."
  (:require [clork.utils :as utils]
            [clork.game-state :as gs]
            [clork.parser.state :as parser-state]))

;;; ---------------------------------------------------------------------------
;;; CONTAINER HELPERS
;;; ---------------------------------------------------------------------------
;;; Shared by open, examine, look-inside commands

(defn openable?
  "Returns true if the object can be opened (is a container or door)."
  [obj]
  (let [flags (or (:flags obj) #{})]
    (or (contains? flags :cont)
        (contains? flags :door))))

(defn already-open?
  "Returns true if the object is already open."
  [obj]
  (contains? (or (:flags obj) #{}) :open))

(defn add-flag
  "Add a flag to an object. Uses gs/set-thing-flag for proper change tracking."
  [game-state obj-id flag]
  (gs/set-thing-flag game-state obj-id flag))

(defn remove-flag
  "Remove a flag from an object. Uses gs/unset-thing-flag for proper change tracking."
  [game-state obj-id flag]
  (gs/unset-thing-flag game-state obj-id flag))

(defn container?
  "Returns true if the object is a container."
  [obj]
  (contains? (or (:flags obj) #{}) :cont))

(defn transparent?
  "Returns true if the object is transparent."
  [obj]
  (contains? (or (:flags obj) #{}) :trans))

(defn surface?
  "Returns true if the object is a surface (like a table)."
  [obj]
  (contains? (or (:flags obj) #{}) :surface))

(defn door?
  "Returns true if the object is a door."
  [obj]
  (contains? (or (:flags obj) #{}) :door))

(defn describe-contents
  "Describe the contents of a container that was just opened.
   ZIL: Uses 'and' before last item: 'a X, and a Y' or 'a X, a Y, and a Z'."
  [game-state obj-id]
  (let [contents (gs/get-contents game-state obj-id)
        visible (remove (fn [id]
                          (gs/set-thing-flag? game-state id :invisible))
                        contents)]
    (if (empty? visible)
      ""
      (let [descriptions (mapv (fn [id]
                                 (let [obj (gs/get-thing game-state id)]
                                   (str "a " (:desc obj))))
                               visible)
            n (count descriptions)]
        (cond
          (= n 1) (first descriptions)
          (= n 2) (str (first descriptions) ", and " (second descriptions))
          :else (str (clojure.string/join ", " (butlast descriptions))
                     ", and " (last descriptions)))))))

;;; ---------------------------------------------------------------------------
;;; LOOK-INSIDE COMMAND
;;; ---------------------------------------------------------------------------
;;; ZIL: V-LOOK-INSIDE in gverbs.zil

(defn v-look-inside
  "Look inside an object (container or door).

   ZIL: V-LOOK-INSIDE in gverbs.zil (line 881)
     <ROUTINE V-LOOK-INSIDE ()
       <COND (<FSET? ,PRSO ,DOORBIT>
              <COND (<FSET? ,PRSO ,OPENBIT>
                     <TELL \"The \" D ,PRSO \" is open, but I can't tell what's beyond it.\">)
                    (T
                     <TELL \"The \" D ,PRSO \" is closed.\">)>
              <CRLF>)
             (<FSET? ,PRSO ,CONTBIT>
              <COND (<FSET? ,PRSO ,ACTORBIT>
                     <TELL \"There is nothing special to be seen.\" CR>)
                    (<SEE-INSIDE? ,PRSO>
                     <COND (<AND <FIRST? ,PRSO> <PRINT-CONT ,PRSO>>
                            <RTRUE>)
                           (T
                            <TELL \"The \" D ,PRSO \" is empty.\" CR>)>)
                    (T
                     <TELL \"The \" D ,PRSO \" is closed.\" CR>)>)
             (T
              <TELL \"You can't look inside a \" D ,PRSO \".\" CR>)>>"
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        obj (gs/get-thing game-state prso)
        desc (:desc obj)
        flags (or (:flags obj) #{})]
    (cond
      ;; Door
      (contains? flags :door)
      (if (gs/set-thing-flag? game-state prso :open)
        (utils/tell game-state (str "The " desc " is open, but I can't tell what's beyond it."))
        (utils/tell game-state (str "The " desc " is closed.")))

      ;; Container
      (contains? flags :cont)
      (cond
        ;; Actor (NPC holding things) - special message
        (contains? flags :actor)
        (utils/tell game-state "There is nothing special to be seen.")

        ;; Can see inside (open or transparent)
        (or (gs/set-thing-flag? game-state prso :open)
            (gs/set-thing-flag? game-state prso :trans))
        (let [contents (gs/get-contents game-state prso)
              visible (remove (fn [id]
                                (gs/set-thing-flag? game-state id :invisible))
                              contents)]
          (if (empty? visible)
            (utils/tell game-state (str "The " desc " is empty."))
            ;; Print contents - each item on its own paragraph
            (let [content-strs (map (fn [id]
                                      (let [o (gs/get-thing game-state id)
                                            suffix (if (gs/set-thing-flag? game-state id :on)
                                                     " (providing light)"
                                                     "")]
                                        (str "A " (:desc o) suffix)))
                                    visible)]
              (-> game-state
                  (utils/tell (str "The " desc " contains:\n"))
                  (utils/tell (clojure.string/join "\n" content-strs))))))

        ;; Closed container
        :else
        (utils/tell game-state (str "The " desc " is closed.")))

      ;; Not a container or door
      :else
      (utils/tell game-state (str "You can't look inside a " desc ".")))))

;;; ---------------------------------------------------------------------------
;;; EXAMINE COMMAND
;;; ---------------------------------------------------------------------------
;;; ZIL: V-EXAMINE in gverbs.zil

(defn v-examine
  "Examine an object to learn more about it.

   ZIL: V-EXAMINE in gverbs.zil (line 639)
     <ROUTINE V-EXAMINE ()
       <COND (<GETP ,PRSO ,P?TEXT>
              <TELL <GETP ,PRSO ,P?TEXT> CR>)
             (<OR <FSET? ,PRSO ,CONTBIT>
                  <FSET? ,PRSO ,DOORBIT>>
              <V-LOOK-INSIDE>)
             (T
              <TELL \"There's nothing special about the \" D ,PRSO \".\" CR>)>>"
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        obj (gs/get-thing game-state prso)
        desc (:desc obj)
        flags (or (:flags obj) #{})
        action-fn (:action obj)]
    ;; First try the object's action handler
    (if-let [result (when action-fn (action-fn game-state))]
      result
      ;; Object didn't handle it - default behavior
      (cond
        ;; Has text property - show it
        (:text obj)
        (utils/tell game-state (:text obj))

        ;; Container or door - look inside
        (or (contains? flags :cont) (contains? flags :door))
        (v-look-inside game-state)

        ;; Default - nothing special
        :else
        (utils/tell game-state (str "There's nothing special about the " desc "."))))))

;;; ---------------------------------------------------------------------------
;;; OPEN COMMAND
;;; ---------------------------------------------------------------------------
;;; ZIL: V-OPEN in gverbs.zil

(defn v-open
  "Open a container or door.

   ZIL: V-OPEN in gverbs.zil
     <ROUTINE V-OPEN ()
       <COND (<FSET? ,PRSO ,CONTBIT>
              <COND (<FSET? ,PRSO ,OPENBIT>
                     <TELL \"It is already open.\" CR>)
                    (T
                     <FSET ,PRSO ,OPENBIT>
                     <FSET ,PRSO ,TOUCHBIT>
                     ...)>)
             (<FSET? ,PRSO ,DOORBIT>
              <COND (<FSET? ,PRSO ,OPENBIT>
                     <TELL \"It is already open.\" CR>)
                    (T
                     <FSET ,PRSO ,OPENBIT>
                     <TELL \"The \" D ,PRSO \" opens.\" CR>)>)
             (T
              <TELL \"You must tell me how to do that to a \" D ,PRSO \".\" CR>)>>"
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        obj (gs/get-thing game-state prso)
        desc (:desc obj)
        action-fn (:action obj)]
    ;; First try the object's action handler (like kitchen window)
    (if-let [result (when action-fn (action-fn game-state))]
      result
      ;; Object didn't handle it - default behavior
      (cond
        ;; Not openable
        (not (openable? obj))
        (utils/tell game-state (str "You must tell me how to do that to a " desc "."))

        ;; Already open
        (already-open? obj)
        (utils/tell game-state "It is already open.")

        ;; Container
        (container? obj)
        (let [state (-> game-state
                        (add-flag prso :open)
                        (add-flag prso :touch))
              contents (gs/get-contents state prso)
              visible (remove (fn [id]
                                (gs/set-thing-flag? state id :invisible))
                              contents)]
          (cond
            ;; Empty or transparent: just "Opened."
            (or (empty? visible) (transparent? obj))
            (utils/tell state "Opened.")

            ;; ZIL: Single untouched item with FDESC: "The X opens." + FDESC
            (and (= 1 (count visible))
                 (let [item-id (first visible)
                       touched? (gs/set-thing-flag? state item-id :touch)]
                   (and (not touched?) (:fdesc (gs/get-thing state item-id)))))
            (let [item (gs/get-thing state (first visible))]
              (-> state
                  (utils/tell (str "The " desc " opens.\n"))
                  (utils/tell (:fdesc item))))

            ;; Otherwise: "Opening the X reveals Y."
            :else
            (let [content-desc (describe-contents state prso)]
              (utils/tell state (str "Opening the " desc " reveals " content-desc ".")))))

        ;; Door
        :else
        (let [state (add-flag game-state prso :open)]
          (utils/tell state (str "The " desc " opens.")))))))

;;; ---------------------------------------------------------------------------
;;; CLOSE COMMAND
;;; ---------------------------------------------------------------------------
;;; ZIL: V-CLOSE in gverbs.zil

(defn v-close
  "Close a container or door.

   ZIL: V-CLOSE in gverbs.zil (line 352)
     <ROUTINE V-CLOSE ()
       <COND (<AND <NOT <FSET? ,PRSO ,CONTBIT>>
                   <NOT <FSET? ,PRSO ,DOORBIT>>>
              <TELL \"You must tell me how to do that to a \" D ,PRSO \".\" CR>)
             (<AND <NOT <FSET? ,PRSO ,SURFACEBIT>>
                   <NOT <EQUAL? <GETP ,PRSO ,P?CAPACITY> 0>>>
              <COND (<FSET? ,PRSO ,OPENBIT>
                     <FCLEAR ,PRSO ,OPENBIT>
                     <TELL \"Closed.\" CR>
                     <COND (<AND ,LIT <NOT <SETG LIT <LIT? ,HERE>>>>
                            <TELL \"It is now pitch black.\" CR>)>
                     <RTRUE>)
                    (T
                     <TELL \"It is already closed.\" CR>)>)
             (<FSET? ,PRSO ,DOORBIT>
              <COND (<FSET? ,PRSO ,OPENBIT>
                     <FCLEAR ,PRSO ,OPENBIT>
                     <TELL \"The \" D ,PRSO \" is now closed.\" CR>)
                    (T
                     <TELL \"It is already closed.\" CR>)>)
             (T
              <TELL \"You cannot close that.\" CR>)>>"
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        obj (gs/get-thing game-state prso)
        desc (:desc obj)
        action-fn (:action obj)]
    ;; First try the object's action handler
    (if-let [result (when action-fn (action-fn game-state))]
      result
      ;; Object didn't handle it - default behavior
      (cond
        ;; Not closable (neither container nor door)
        (not (openable? obj))
        (utils/tell game-state (str "You must tell me how to do that to a " desc "."))

        ;; Surface - can't close a surface like a table
        (surface? obj)
        (utils/tell game-state "You cannot close that.")

        ;; Container (not a surface)
        (container? obj)
        (if (already-open? obj)
          (let [state (remove-flag game-state prso :open)]
            ;; TODO: Check if room becomes dark and add "It is now pitch black."
            (utils/tell state "Closed."))
          (utils/tell game-state "It is already closed."))

        ;; Door
        (door? obj)
        (if (already-open? obj)
          (let [state (remove-flag game-state prso :open)]
            (utils/tell state (str "The " desc " is now closed.")))
          (utils/tell game-state "It is already closed."))

        ;; Fallback (shouldn't reach here given openable? check)
        :else
        (utils/tell game-state "You cannot close that.")))))

;;; ---------------------------------------------------------------------------
;;; LOOK UNDER COMMAND
;;; ---------------------------------------------------------------------------
;;; ZIL: V-LOOK-UNDER in gverbs.zil

(defn v-look-under
  "Look under an object.

   ZIL: V-LOOK-UNDER in gverbs.zil (line 915)
     <ROUTINE V-LOOK-UNDER ()
       <TELL \"There is nothing but dust there.\" CR>>

   Note: The default implementation just shows a generic message.
   Specific objects can override this via their action handlers."
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        obj (gs/get-thing game-state prso)
        action-fn (:action obj)]
    ;; First try the object's action handler
    (if-let [result (when action-fn (action-fn game-state))]
      result
      ;; Default behavior
      (utils/tell game-state "There is nothing but dust there."))))

;;; ---------------------------------------------------------------------------
;;; LOOK BEHIND COMMAND
;;; ---------------------------------------------------------------------------
;;; ZIL: V-LOOK-BEHIND in gverbs.zil

(defn v-look-behind
  "Look behind an object.

   ZIL: V-LOOK-BEHIND in gverbs.zil (line 878)
     <ROUTINE V-LOOK-BEHIND ()
       <TELL \"There is nothing behind the \" D ,PRSO \".\" CR>>

   Note: The default implementation just shows a generic message.
   Specific objects can override this via their action handlers."
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        obj (gs/get-thing game-state prso)
        desc (:desc obj)
        action-fn (:action obj)]
    ;; First try the object's action handler
    (if-let [result (when action-fn (action-fn game-state))]
      result
      ;; Default behavior
      (utils/tell game-state (str "There is nothing behind the " desc ".")))))

;;; ---------------------------------------------------------------------------
;;; LOOK ON COMMAND
;;; ---------------------------------------------------------------------------
;;; ZIL: V-LOOK-ON in gverbs.zil

(defn v-look-on
  "Look on top of an object (surface).

   ZIL: V-LOOK-ON in gverbs.zil (line 908)
     <ROUTINE V-LOOK-ON ()
       <COND (<FSET? ,PRSO ,SURFACEBIT>
              <PERFORM ,V?LOOK-INSIDE ,PRSO>
              <RTRUE>)
             (T
              <TELL \"Look on a \" D ,PRSO \"???\" CR>)>>

   If the object is a surface, delegates to v-look-inside.
   Otherwise shows a confused message."
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        obj (gs/get-thing game-state prso)
        desc (:desc obj)
        action-fn (:action obj)]
    ;; First try the object's action handler
    (if-let [result (when action-fn (action-fn game-state))]
      result
      ;; Default behavior
      (if (surface? obj)
        ;; Surface - delegate to v-look-inside
        (v-look-inside game-state)
        ;; Not a surface - confused message
        (utils/tell game-state (str "Look on a " desc "???"))))))
