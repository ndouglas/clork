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
  "Add a flag to an object's flag set in game-state."
  [game-state obj-id flag]
  (let [current-flags (get-in game-state [:objects obj-id :flags] #{})]
    (assoc-in game-state [:objects obj-id :flags] (conj current-flags flag))))

(defn container?
  "Returns true if the object is a container."
  [obj]
  (contains? (or (:flags obj) #{}) :cont))

(defn transparent?
  "Returns true if the object is transparent."
  [obj]
  (contains? (or (:flags obj) #{}) :trans))

(defn describe-contents
  "Describe the contents of a container that was just opened."
  [game-state obj-id]
  (let [contents (gs/get-contents game-state obj-id)
        visible (remove (fn [id]
                          (let [obj (gs/get-thing game-state id)
                                flags (or (:flags obj) #{})]
                            (contains? flags :invisible)))
                        contents)]
    (if (empty? visible)
      ""
      (let [descriptions (map (fn [id]
                                (let [obj (gs/get-thing game-state id)]
                                  (str "a " (:desc obj))))
                              visible)]
        (clojure.string/join ", " descriptions)))))

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
      (if (contains? flags :open)
        (utils/tell game-state (str "The " desc " is open, but I can't tell what's beyond it."))
        (utils/tell game-state (str "The " desc " is closed.")))

      ;; Container
      (contains? flags :cont)
      (cond
        ;; Actor (NPC holding things) - special message
        (contains? flags :actor)
        (utils/tell game-state "There is nothing special to be seen.")

        ;; Can see inside (open or transparent)
        (or (contains? flags :open) (contains? flags :trans))
        (let [contents (gs/get-contents game-state prso)
              visible (remove (fn [id]
                                (let [o (gs/get-thing game-state id)
                                      oflags (or (:flags o) #{})]
                                  (contains? oflags :invisible)))
                              contents)]
          (if (empty? visible)
            (utils/tell game-state (str "The " desc " is empty."))
            ;; Print contents
            (let [content-strs (map (fn [id]
                                      (let [o (gs/get-thing game-state id)]
                                        (str "  A " (:desc o))))
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
        flags (or (:flags obj) #{})]
    (cond
      ;; Has text property - show it
      (:text obj)
      (utils/tell game-state (:text obj))

      ;; Container or door - look inside
      (or (contains? flags :cont) (contains? flags :door))
      (v-look-inside game-state)

      ;; Default - nothing special
      :else
      (utils/tell game-state (str "There's nothing special about the " desc ".")))))

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
                                (let [o (gs/get-thing state id)
                                      flags (or (:flags o) #{})]
                                  (contains? flags :invisible)))
                              contents)]
          (if (or (empty? visible) (transparent? obj))
            (utils/tell state "Opened.")
            (let [content-desc (describe-contents state prso)]
              (utils/tell state (str "Opening the " desc " reveals " content-desc ".")))))

        ;; Door
        :else
        (let [state (add-flag game-state prso :open)]
          (utils/tell state (str "The " desc " opens.")))))))
