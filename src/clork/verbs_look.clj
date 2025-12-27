(ns clork.verbs-look
  "Look command implementation."
  (:require [clork.utils :as utils]
            [clork.game-state :as game-state]))

;; Aliases for functions used in this module
(def tell utils/tell)
(def get-here game-state/get-here)
(def get-winner-loc game-state/get-winner-loc)
(def verbose? game-state/verbose?)
(def set-here-flag game-state/set-here-flag)
(def set-here-flag? game-state/set-here-flag?)
(def unset-here-flag game-state/unset-here-flag)

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
  ([game-state] (describe-room game-state (get-winner-loc game-state) false))
  ([game-state location is-verbose?]
   (let [is-verbose? (or is-verbose? (verbose? game-state))
         lit? (set-here-flag? game-state :lit)
         maze? (set-here-flag? game-state :maze)
         is-verbose? (if maze? is-verbose? true)
         vehicle? (get-in location [:flags :vehicle] false)
         here (get-here game-state)
         act (:action here)]
     (if (not lit?)
       ;; Dark room - tell them and return
       (tell game-state "It is pitch black. You are likely to be eaten by a grue.")
       ;; Lit room - describe it, threading state through each operation
       (let [gs (-> game-state
                    (set-here-flag :touch)
                    (cond-> maze? (unset-here-flag :touch))
                    (cond-> vehicle? (tell (str "(You are in the " (:desc location) ".)"))))
             gs (cond
                  (and is-verbose? (some? act)) (act gs :look)
                  is-verbose? (tell gs (:ldesc here))
                  (some? act) (act gs :flash)
                  :else gs)
             gs (if (and vehicle? (some? act) (not= (:id location) (:id here)))
                  (act gs :look)
                  gs)]
         gs)))))

(defn describe-objects
  "Describes the objects in the room."
  ([game-state] (describe-objects game-state false))
  ([game-state look] game-state))

(defn v-look
  "Describes the room and any objects."
  ([game-state] (v-look game-state (get-winner-loc game-state)))
  ([game-state location]
   (-> game-state
       (describe-room location true)
       (describe-objects true))))
