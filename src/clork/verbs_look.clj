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
   (let [;; ZIL: Brief mode shows full description on first visit, only brief on return visits
         ;; Check :touch flag BEFORE we set it later - if not touched, this is first visit
         first-visit? (not (gs/set-here-flag? game-state :touch))
         is-verbose? (or is-verbose? (gs/verbose? game-state) first-visit?)
         ;; Check global :lit flag (set by goto based on room + light sources)
         ;; OR the room's native :lit flag
         lit? (or (:lit game-state) (gs/set-here-flag? game-state :lit))
         maze? (gs/set-here-flag? game-state :maze)
         ;; ZIL: Maze rooms always show full description (touch is cleared each visit)
         ;; Non-maze rooms use the current verbose setting
         is-verbose? (if maze? true is-verbose?)
         vehicle? (get-in location [:flags :vehicle] false)
         here (gs/get-here game-state)
         ;; Check if vehicle is beached (in the current room, not on water)
         vehicle-loc (when vehicle? (gs/get-thing-loc-id game-state (:id location)))
         beached-vehicle? (and vehicle? (= vehicle-loc (:here game-state)))
         act (:action here)]
     (if (not lit?)
       ;; Dark room - tell them and return (ZIL: <RFALSE> after this)
       (-> game-state
           (utils/tell "It is pitch black. You are likely to be eaten by a grue.\n"))
       ;; Lit room - describe it, threading state through each operation
       ;; ZIL: <TELL D ,HERE> <CRLF> -- print room name first
       ;; Use double newline for paragraph separation from description
       (let [room-name (:desc here)
             vehicle-desc (when vehicle? (:desc location))
             ;; For beached boat: "Room Name, in the magic boat"
             ;; For floating boat: just room name (handled elsewhere)
             ;; For non-vehicle: just room name
             header (if beached-vehicle?
                      (str room-name ", in the " vehicle-desc)
                      room-name)
             state (-> game-state
                       (cond-> header (utils/tell header))
                       (cond-> header (utils/tell "\n"))
                       (gs/set-here-flag :touch)
                       (cond-> maze? (gs/unset-here-flag :touch))
                       ;; Only show "(You are in the X.)" if NOT beached
                       (cond-> (and vehicle? (not beached-vehicle?))
                         (utils/tell (str "(You are in the " vehicle-desc ".)"))))
             ;; ZIL pattern: action returns game-state with :use-default-handling to signal fallback
             ;; If action signals use-default or doesn't exist, fall back to :ldesc
             state (cond
                     ;; Verbose mode with action - try action first, fall back to ldesc
                     (and is-verbose? (some? act))
                     (let [result (act state :look)]
                       (if (gs/use-default? result)
                         (if-let [ldesc (:ldesc here)]
                           ;; Use paragraph break after room description
                           (-> (gs/clear-use-default result) (utils/tell ldesc) (utils/tell "\n"))
                           (gs/clear-use-default result))
                         result))
                     ;; Verbose mode without action - use ldesc (guard against nil ldesc)
                     (and is-verbose? (:ldesc here))
                     ;; Use paragraph break after room description
                     (-> state (utils/tell (:ldesc here)) (utils/tell "\n"))
                     ;; Non-verbose with action - try flash (no ldesc fallback for flash)
                     (some? act)
                     (let [result (act state :flash)]
                       (if (gs/use-default? result)
                         (gs/clear-use-default result)
                         result))
                     :else state)
             state (if (and vehicle? (some? act) (not= (:id location) (:id here)))
                     (let [result (act state :look)]
                       (if (gs/use-default? result)
                         (gs/clear-use-default result)
                         result))
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
  "Returns true if we can see inside an object (transparent or open, not invisible).
   Uses gs/set-thing-flag? which checks both direct keys and the :flags set."
  [game-state obj-id]
  (and (not (gs/set-thing-flag? game-state obj-id :invisible))
       (or (gs/set-thing-flag? game-state obj-id :trans)
           (gs/set-thing-flag? game-state obj-id :open))))

(defn- has-flag?
  "Check if an object has a specific flag.
   Uses gs/set-thing-flag? which checks both direct keys and the :flags set."
  [game-state obj-id flag]
  (gs/set-thing-flag? game-state obj-id flag))

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
        winner (:winner game-state)
        ;; Single newline - all room content is same paragraph
        line-end "\n"]
    (cond
      ;; Trophy case special message
      (= obj-id :trophy-case)
      (utils/tell game-state (str "Your collection of treasures consists of:" line-end))

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
          (utils/tell state (str "Sitting on the " desc " is:" line-end))

          (has-flag? game-state obj-id :actor)
          (utils/tell state (str "The " desc " is holding:" line-end))

          :else
          (utils/tell state (str "The " desc " contains:" line-end)))))))

;;; ---------------------------------------------------------------------------
;;; DESCRIBE-OBJECT
;;; ---------------------------------------------------------------------------
;; ZIL: <ROUTINE DESCRIBE-OBJECT (OBJ V? LEVEL ...)>

(declare print-cont)

(defn describe-object
  "Describe a single object. Level 0 is room floor, >0 is inside containers."
  [game-state obj-id verbose? level]
  (let [obj (gs/get-thing game-state obj-id)
        desc (:desc obj)]
    ;; Check for custom description function first
    (if-let [descfcn (:descfcn obj)]
      (descfcn game-state :objdesc)
      ;; Standard description logic
      ;; Use gs/set-thing-flag? for all flag checks to handle runtime flag changes
      (let [touched? (gs/set-thing-flag? game-state obj-id :touch)
            on? (gs/set-thing-flag? game-state obj-id :on)
            wear? (gs/set-thing-flag? game-state obj-id :wear)
            fdesc (:fdesc obj)
            ldesc (:ldesc obj)
            ;; ZIL: Check if player is in a vehicle (for "(outside the X)" suffix)
            ;; Lines 1738-1741: <COND (<AND <0? .LEVEL> <SET AV <LOC ,WINNER>> <FSET? .AV ,VEHBIT>>
            winner (:winner game-state)
            player-loc (gs/get-thing-loc-id game-state winner)
            in-vehicle? (and (keyword? player-loc)
                             (gs/set-thing-flag? game-state player-loc :vehicle))
            vehicle-desc (when in-vehicle?
                           (:desc (gs/get-thing game-state player-loc)))
            ;; Helper to add vehicle suffix at level 0
            add-vehicle-suffix (fn [state]
                                 (if (and (zero? level) in-vehicle?)
                                   (utils/tell state (str " (outside the " vehicle-desc ")"))
                                   state))
            ;; Helper to add contents description if visible
            add-contents (fn [state]
                           (if (and (see-inside? game-state obj-id)
                                    (seq (gs/get-contents game-state obj-id)))
                             (print-cont state obj-id verbose? level)
                             state))]
        (cond
          ;; Level 0 with fdesc (first description) or ldesc
          ;; ZIL (lines 1714-1718): When using FDESC/LDESC, just print the string
          ;; NO "(providing light)" annotation - that's only for generic descriptions
          (and (zero? level)
               (or (and (not touched?) fdesc)
                   ldesc))
          (let [str (if (and (not touched?) fdesc) fdesc ldesc)
                state (utils/tell game-state str)]
            (-> state
                add-vehicle-suffix
                ;; Double newline for paragraph separation at room floor level
                (utils/tell "\n")
                add-contents))

          ;; Level 0, generic description
          ;; ZIL (line 1719-1722): "There is a X here" + "(providing light)" if ONBIT
          ;; Then lines 1738-1741: "(outside the X)" if in vehicle
          (zero? level)
          (let [state (utils/tell game-state (str "There is " (get-article game-state obj-id) desc " here"))
                state (if on?
                        (utils/tell state " (providing light)")
                        state)
                state (utils/tell state ".")]
            (-> state
                add-vehicle-suffix
                (utils/tell "\n")
                add-contents))

          ;; Level 1: first container contents (no indent, paragraph breaks)
          (= level 1)
          (let [state (utils/tell game-state (str "A " desc))
                state (cond
                        on?
                        (utils/tell state " (providing light)")

                        (and wear?
                             (= (:in obj) (:winner game-state)))
                        (utils/tell state " (being worn)")

                        :else state)]
            (-> state
                (utils/tell "\n")
                add-contents))

          ;; Nested level 2+ (inside nested containers - indented, single newline)
          :else
          (let [state (utils/tell game-state (get-indent level))
                state (utils/tell state (str "A " desc))
                state (cond
                        on?
                        (utils/tell state " (providing light)")

                        (and wear?
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
         ;; ZIL: Check if player is in a vehicle - skip vehicle from room contents
         ;; Lines 1791, 1813: (<EQUAL? .Y .AV>) - skip the vehicle when iterating
         player-loc (gs/get-thing-loc-id game-state winner)
         player-vehicle (when (and (keyword? player-loc)
                                   (gs/set-thing-flag? game-state player-loc :vehicle))
                          player-loc)
         ;; Filter out invisible objects, the player, and the player's vehicle
         visible-contents (remove (fn [id]
                                    (or (= id winner)
                                        (= id player-vehicle)
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

             ;; Print fdesc objects first (with paragraph breaks at room floor level)
             state (reduce (fn [st id]
                             (let [obj (gs/get-thing game-state id)]
                               (-> st
                                   (utils/tell (:fdesc obj))
                                   ;; Single newline - all room content is same paragraph
                                   (utils/tell "\n")
                                   ;; Also print contents if visible
                                   ;; Pass 0 for container contents (level 0 gets paragraph breaks in firster)
                                   (cond-> (and (see-inside? st id)
                                                (seq (gs/get-contents st id)))
                                     (print-cont id verbose? 0)))))
                           game-state
                           with-fdesc)

             ;; Second pass: remaining objects
             ;; Separate into describable (no :ndesc) and ndesc containers
             describable (remove (fn [id] (has-flag? game-state id :ndesc))
                                 without-fdesc)
             ;; ZIL: Objects with NDESCBIT that have visible contents still
             ;; get their contents described (lines 1827-1830 in gverbs.zil)
             ndesc-containers (filter (fn [id]
                                        (and (has-flag? game-state id :ndesc)
                                             (see-inside? game-state id)
                                             (seq (gs/get-contents game-state id))))
                                      without-fdesc)
             ;; Special handling for surfaces: touched objects should be described
             ;; at level 0 ("There is a X here.") rather than under "Sitting on..." header.
             ;; This matches ZIL behavior where touched objects "lose" their surface context.
             is-surface? (has-flag? game-state obj-id :surface)
             ;; For surfaces, split describable into touched and untouched
             touched-on-surface (when (and is-surface? (= level 0))
                                  (filter (fn [id] (has-flag? game-state id :touch)) describable))
             untouched-on-surface (if touched-on-surface
                                    (remove (fn [id] (has-flag? game-state id :touch)) describable)
                                    describable)]

         ;; First describe regular objects, then ndesc containers' contents
         ;; ZIL describes objects before container contents
         (let [state (if (empty? untouched-on-surface)
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
                                 untouched-on-surface)))
               ;; Touched objects on surfaces get described at level 0 without header
               state (if (seq touched-on-surface)
                       (reduce (fn [st id]
                                 (describe-object st id verbose? 0))
                               state
                               touched-on-surface)
                       state)]
           ;; Then describe ndesc containers' contents (like trophy case)
           ;; Pass level 0 so firster gets called (e.g. "Your collection of treasures consists of:")
           (let [state (reduce (fn [st id]
                                 (print-cont st id verbose? 0))
                               state
                               ndesc-containers)]
             ;; ZIL lines 1809-1811: After room contents, if player is in a vehicle,
             ;; print the vehicle's contents (e.g., "The magic boat contains:")
             (if (and player-vehicle
                      (= level -1)  ;; Only at room floor level
                      (see-inside? game-state player-vehicle)
                      (seq (gs/get-contents game-state player-vehicle)))
               (print-cont state player-vehicle verbose? 0)
               state))))))))

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
