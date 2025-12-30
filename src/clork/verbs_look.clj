(ns clork.verbs-look
  "Look command implementation."
  (:require [clork.utils :as utils]
            [clork.game-state :as gs]))

;;  <ROUTINE DESCRIBE-ROOM ("OPTIONAL" (LOOK? <>) "AUX" V? STR AV)
;;    <SET V? <OR .LOOK? ,VERBOSE>>
;;  	<COND (<NOT ,LIT>
;;      <TELL "It is pitch black.">
;;  	  <COND (<NOT ,SPRAYED?>
;;        <TELL " You are likely to be eaten by a grue.">
;;      )>
;;      <CRLF>
;;      %<COND (<==? ,ZORK-NUMBER 3>
;;  		  '<COND (<EQUAL? ,HERE ,DARK-2>
;;          <TELL "The ground continues to slope upwards away from the lake. You can barely detect a dim light from the east." CR>
;;        )>
;;      )
;;      (T
;;        '<NULL-F>
;;      )>
;;  	  <RFALSE>
;;    )>
;;  	<COND (<NOT <FSET? ,HERE ,TOUCHBIT>>
;;  		<FSET ,HERE ,TOUCHBIT>
;;  		<SET V? T>
;;    )>
;;  	 %<COND (<==? ,ZORK-NUMBER 1>
;;  		 '<COND (<FSET? ,HERE ,MAZEBIT>
;;  		         <FCLEAR ,HERE ,TOUCHBIT>)>)
;;  		(T
;;  		 '<NULL-F>)>
;;  	 <COND (<IN? ,HERE ,ROOMS>
;;  		;"Was <TELL D ,HERE CR>"
;;  		<TELL D ,HERE>
;;  		<COND (<FSET? <SET AV <LOC ,WINNER>> ,VEHBIT>
;;  		       <TELL ", in the " D .AV>)>
;;  		<CRLF>)>
;;  	 <COND (%<COND (<==? ,ZORK-NUMBER 2>
;;  			'<OR .LOOK? <NOT ,SUPER-BRIEF> <EQUAL? ,HERE ,ZORK3>>)
;;  		       (ELSE
;;  			'<OR .LOOK? <NOT ,SUPER-BRIEF>>)>
;;  		<SET AV <LOC ,WINNER>>
;;  		;<COND (<FSET? .AV ,VEHBIT>
;;  		       <TELL "(You are in the " D .AV ".)" CR>)>
;;  		<COND (<AND .V? <APPLY <GETP ,HERE ,P?ACTION> ,M-LOOK>>
;;  		       <RTRUE>)
;;  		      (<AND .V? <SET STR <GETP ,HERE ,P?LDESC>>>
;;  		       <TELL .STR CR>)
;;  		      (T
;;  		       <APPLY <GETP ,HERE ,P?ACTION> ,M-FLASH>)>
;;  		<COND (<AND <NOT <EQUAL? ,HERE .AV>> <FSET? .AV ,VEHBIT>>
;;  		       <APPLY <GETP .AV ,P?ACTION> ,M-LOOK>)>)>
;;  	 T>

(defn describe-room
  "Describes the room."
  ([game-state] (describe-room game-state (gs/get-winner-loc game-state) false))
  ([game-state location is-verbose?]
   (let [is-verbose? (or is-verbose? (gs/verbose? game-state))
         ;; Check global :lit flag (set by goto based on room + light sources)
         ;; OR the room's native :lit flag
         lit? (or (:lit game-state) (gs/set-here-flag? game-state :lit))
         maze? (gs/set-here-flag? game-state :maze)
         is-verbose? (if maze? is-verbose? true)
         vehicle? (get-in location [:flags :vehicle] false)
         here (gs/get-here game-state)
         act (:action here)]
     (if (not lit?)
       ;; Dark room - tell them and return (ZIL: <RFALSE> after this)
       (-> game-state
           (utils/tell "It is pitch black. You are likely to be eaten by a grue.\n"))
       ;; Lit room - describe it, threading state through each operation
       ;; ZIL: <TELL D ,HERE> <CRLF> -- print room name first
       (let [room-name (:desc here)
             state (-> game-state
                       (cond-> room-name (utils/tell room-name))
                       (cond-> room-name (utils/tell "\n"))
                       (gs/set-here-flag :touch)
                       (cond-> maze? (gs/unset-here-flag :touch))
                       (cond-> vehicle? (utils/tell (str "(You are in the " (:desc location) ".)"))))
             state (cond
                     (and is-verbose? (some? act)) (act state :look)
                     is-verbose? (-> state (utils/tell (:ldesc here)) (utils/crlf))
                     (some? act) (act state :flash)
                     :else state)
             state (if (and vehicle? (some? act) (not= (:id location) (:id here)))
                     (act state :look)
                     state)]
         state)))))

;;; ---------------------------------------------------------------------------
;;; OBJECT DESCRIPTION HELPERS
;;; ---------------------------------------------------------------------------

;; ZIL: <GLOBAL INDENTS <TABLE "" "  " "    " ...>>
(def indents
  "Indentation strings for nested container contents."
  ["" "  " "    " "      " "        " "          "])

(defn- get-indent
  "Get indentation string for a nesting level."
  [level]
  (get indents (min level (dec (count indents)))))

;; ZIL: <ROUTINE SEE-INSIDE? (OBJ)
;;        <AND <NOT <FSET? .OBJ ,INVISIBLE>>
;;             <OR <FSET? .OBJ ,TRANSBIT> <FSET? .OBJ ,OPENBIT>>>>
(defn see-inside?
  "Returns true if we can see inside an object (transparent or open, not invisible)."
  [game-state obj-id]
  (let [obj (gs/get-thing game-state obj-id)
        flags (or (:flags obj) #{})]
    (and (not (contains? flags :invisible))
         (or (contains? flags :trans)
             (contains? flags :open)))))

(defn- has-flag?
  "Check if an object has a specific flag."
  [game-state obj-id flag]
  (let [obj (gs/get-thing game-state obj-id)
        flags (or (:flags obj) #{})]
    (contains? flags flag)))

(defn- get-article
  "Get the appropriate article for an object."
  [game-state obj-id]
  (cond
    (has-flag? game-state obj-id :narticle) ""
    (has-flag? game-state obj-id :plural) "some "
    (has-flag? game-state obj-id :vowel) "an "
    :else "a "))

;; ZIL: <ROUTINE FIRSTER (OBJ LEVEL)>
(defn- firster
  "Print the header for container contents."
  [game-state obj-id level]
  (let [obj (gs/get-thing game-state obj-id)
        desc (:desc obj)
        winner (:winner game-state)]
    (cond
      ;; Trophy case special message
      (= obj-id :trophy-case)
      (utils/tell game-state "Your collection of treasures consists of:\n")

      ;; Player inventory
      (= obj-id winner)
      (utils/tell game-state "You are carrying:\n")

      ;; Container/surface/actor in a room
      :else
      (let [state (if (> level 0)
                    (utils/tell game-state (get-indent level))
                    game-state)]
        (cond
          (has-flag? game-state obj-id :surface)
          (utils/tell state (str "Sitting on the " desc " is:\n"))

          (has-flag? game-state obj-id :actor)
          (utils/tell state (str "The " desc " is holding:\n"))

          :else
          (utils/tell state (str "The " desc " contains:\n")))))))

;;; ---------------------------------------------------------------------------
;;; DESCRIBE-OBJECT
;;; ---------------------------------------------------------------------------
;; ZIL: <ROUTINE DESCRIBE-OBJECT (OBJ V? LEVEL ...)>

(declare print-cont)

(defn describe-object
  "Describe a single object. Level 0 is room floor, >0 is inside containers."
  [game-state obj-id verbose? level]
  (let [obj (gs/get-thing game-state obj-id)
        desc (:desc obj)
        flags (or (:flags obj) #{})]
    ;; Check for custom description function first
    (if-let [descfcn (:descfcn obj)]
      (descfcn game-state :objdesc)
      ;; Standard description logic
      (let [touched? (contains? flags :touch)
            fdesc (:fdesc obj)
            ldesc (:ldesc obj)
            ;; Helper to add contents description if visible
            add-contents (fn [state]
                           (if (and (see-inside? game-state obj-id)
                                    (seq (gs/get-contents game-state obj-id)))
                             (print-cont state obj-id verbose? level)
                             state))]
        (cond
          ;; Level 0 with fdesc (first description) or ldesc
          (and (zero? level)
               (or (and (not touched?) fdesc)
                   ldesc))
          (let [str (if (and (not touched?) fdesc) fdesc ldesc)
                state (utils/tell game-state str)
                ;; Add lighting annotation
                state (if (contains? flags :on)
                        (utils/tell state " (providing light)")
                        state)]
            (-> state
                (utils/tell "\n")
                add-contents))

          ;; Level 0, generic description
          (zero? level)
          (let [state (utils/tell game-state (str "There is " (get-article game-state obj-id) desc " here"))
                state (if (contains? flags :on)
                        (utils/tell state " (providing light)")
                        state)]
            (-> state
                (utils/tell ".\n")
                add-contents))

          ;; Nested level (inside container)
          :else
          (let [state (utils/tell game-state (get-indent level))
                state (utils/tell state (str "A " desc))
                state (cond
                        (contains? flags :on)
                        (utils/tell state " (providing light)")

                        (and (contains? flags :wear)
                             (= (:in obj) (:winner game-state)))
                        (utils/tell state " (being worn)")

                        :else state)]
            (-> state
                (utils/tell "\n")
                add-contents)))))))

;;; ---------------------------------------------------------------------------
;;; PRINT-CONT
;;; ---------------------------------------------------------------------------
;; ZIL: <ROUTINE PRINT-CONT (OBJ "OPTIONAL" (V? <>) (LEVEL 0) ...)>

(defn print-cont
  "Print contents of a container/room. Returns updated game-state.
   Level -1 means room floor (no header), level 0+ means container."
  ([game-state obj-id] (print-cont game-state obj-id false 0))
  ([game-state obj-id verbose?] (print-cont game-state obj-id verbose? 0))
  ([game-state obj-id verbose? level]
   (let [contents (gs/get-contents game-state obj-id)
         winner (:winner game-state)
         ;; Filter out invisible objects and the player
         visible-contents (remove (fn [id]
                                    (or (= id winner)
                                        (has-flag? game-state id :invisible)))
                                  contents)]
     (if (empty? visible-contents)
       game-state
       ;; First pass: objects with fdesc that haven't been touched
       (let [;; Separate objects by whether they have untouched fdesc
             has-untouched-fdesc? (fn [id]
                                    (let [obj (gs/get-thing game-state id)]
                                      (and (not (has-flag? game-state id :touch))
                                           (:fdesc obj)
                                           (not (has-flag? game-state id :ndesc)))))
             with-fdesc (filter has-untouched-fdesc? visible-contents)
             without-fdesc (remove has-untouched-fdesc? visible-contents)

             ;; Print fdesc objects first
             state (reduce (fn [st id]
                             (let [obj (gs/get-thing game-state id)]
                               (-> st
                                   (utils/tell (:fdesc obj))
                                   (utils/tell "\n")
                                   ;; Also print contents if visible
                                   (cond-> (and (see-inside? st id)
                                                (seq (gs/get-contents st id)))
                                     (print-cont id verbose? 0)))))
                           game-state
                           with-fdesc)

             ;; Second pass: remaining objects
             describable (remove (fn [id] (has-flag? game-state id :ndesc))
                                 without-fdesc)]

         (if (empty? describable)
           state
           ;; Print header for container (but not for room floor at level -1)
           (let [state (if (and (not= obj-id (:here game-state))
                                (>= level 0))
                         (firster state obj-id level)
                         state)
                 ;; Room floor (level -1) -> describe at level 0
                 ;; Container (level >= 0) -> describe at level+1
                 new-level (if (< level 0) 0 (inc level))]
             (reduce (fn [st id]
                       (describe-object st id verbose? new-level))
                     state
                     describable))))))))

;;; ---------------------------------------------------------------------------
;;; DESCRIBE-OBJECTS
;;; ---------------------------------------------------------------------------
;; ZIL: <ROUTINE DESCRIBE-OBJECTS ("OPTIONAL" (V? <>))>

(defn describe-objects
  "Describes the objects in the current room."
  ([game-state] (describe-objects game-state false))
  ([game-state look?]
   (let [lit? (or (:lit game-state) (gs/set-here-flag? game-state :lit))
         verbose? (or look? (:verbose game-state))
         here (:here game-state)]
     (if lit?
       (let [contents (gs/get-contents game-state here)]
         (if (seq contents)
           (print-cont game-state here verbose? -1)
           game-state))
       (utils/tell game-state "Only bats can see in the dark. And you're not one.\n")))))

;; ZIL: <ROUTINE V-FIRST-LOOK ()
;;        <COND (<DESCRIBE-ROOM>
;;               <COND (<NOT ,SUPER-BRIEF>
;;                      <DESCRIBE-OBJECTS>)>)>>
;;
;; V-FIRST-LOOK is called when entering a room.
;; It only describes objects if the room was described (i.e., lit).
(defn v-first-look
  "Describes the room when entering. Only describes objects if lit.
   ZIL: V-FIRST-LOOK in gverbs.zil"
  ([game-state] (v-first-look game-state (gs/get-winner-loc game-state)))
  ([game-state location]
   (let [lit? (or (:lit game-state) (gs/set-here-flag? game-state :lit))
         state (describe-room game-state location false)]
     (if (and lit? (not (:super-brief game-state)))
       (describe-objects state false)
       state))))

;; ZIL: <ROUTINE V-LOOK ()
;;        <COND (<DESCRIBE-ROOM T>
;;               <DESCRIBE-OBJECTS T>)>>
;;
;; V-LOOK is called for explicit LOOK command.
;; It only describes objects if the room was described (i.e., lit).
(defn v-look
  "Describes the room and any objects for explicit LOOK command.
   ZIL: V-LOOK in gverbs.zil"
  ([game-state] (v-look game-state (gs/get-winner-loc game-state)))
  ([game-state location]
   (let [lit? (or (:lit game-state) (gs/set-here-flag? game-state :lit))
         state (describe-room game-state location true)]
     (if lit?
       (describe-objects state true)
       state))))
