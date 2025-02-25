(in-ns 'clork.core)

;; <OBJECT MAILBOX
;;	(IN WEST-OF-HOUSE)
;;	(SYNONYM MAILBOX BOX)
;;	(ADJECTIVE SMALL)
;;	(DESC "small mailbox")
;;	(FLAGS CONTBIT TRYTAKEBIT)
;;	(CAPACITY 10)
;;	(ACTION MAILBOX-F)>
;;
;; <ROUTINE MAILBOX-F ()
;; 	 <COND (<AND <VERB? TAKE> <EQUAL? ,PRSO ,MAILBOX>>
;; 		 <TELL "It is securely anchored." CR>)>>

(def mailbox {
  :id :mailbox,
  :in :west-of-house,
  :synonym ["mailbox" "box"],
  :adjective "small",
  :desc "small mailbox",
  :flags (flags :cont, :trytake),
  :capacity 10,
  :action (fn [game-state]
    (if
      (and (= (:verb game-state) :take) (= (:prs0 game-state) :mailbox))
      (tell game-state "It is securely anchored.\n")
      game-state))})

;; <OBJECT ADVENTURER
;; 	(SYNONYM ADVENTURER)
;; 	(DESC "cretin")
;; 	(FLAGS NDESCBIT INVISIBLE SACREDBIT ACTORBIT)
;; 	(STRENGTH 0)
;; 	(ACTION 0)>

(def adventurer {
  :id :adventurer,
  :synonym ["adventurer"],
  :desc "cretin",
  :flags (flags :ndesc :invisible :sacred :actor),
  :strength 0})
