(ns clork.debug.thief
  "Debug commands for thief observation.

   Commands:
   - $thief         - Show thief's current status
   - $thief locate  - Where is the thief?
   - $thief bag     - What has the thief stolen?
   - $thief move    - Manually move thief to a room
   - $thief reveal  - Make thief visible"
  (:require [clork.utils :as utils]
            [clork.game-state :as gs]
            [clojure.string :as str]))

;;; ---------------------------------------------------------------------------
;;; HELPERS
;;; ---------------------------------------------------------------------------

(defn- tell-line
  "Output a line with key-value formatting."
  [game-state label value]
  (utils/tell game-state (str "  " label ": " value "\n")))

(defn- format-keyword
  "Format keyword for display."
  [kw]
  (if (keyword? kw) (name kw) (str kw)))

;;; ---------------------------------------------------------------------------
;;; $thief (status)
;;; ---------------------------------------------------------------------------

(defn cmd-thief-status
  "Show comprehensive thief status."
  [game-state _args]
  (let [thief (gs/get-thing game-state :thief)
        thief-loc (gs/get-thing-loc-id game-state :thief)
        loc-room (gs/get-thing game-state thief-loc)
        here (:here game-state)
        stiletto-loc (gs/get-thing-loc-id game-state :stiletto)
        bag-contents (gs/get-contents game-state :thief)
        daemon (get-in game-state [:daemons :i-thief])
        invisible? (gs/set-thing-flag? game-state :thief :invisible)
        fighting? (gs/set-thing-flag? game-state :thief :fight)
        thief-here? (:thief-here game-state)
        strength (get thief :strength 5)]
    (-> game-state
        (utils/tell "=== THIEF STATUS ===\n")
        (tell-line "Location" (str thief-loc " (" (:desc loc-room "?") ")"))
        (tell-line "Player at" (str here (if (= here thief-loc) " [SAME ROOM]" "")))
        (tell-line "Visible" (if invisible? "NO (invisible)" "YES"))
        (tell-line "Fighting" (if fighting? "YES" "NO"))
        (tell-line "Thief-here flag" (if thief-here? "YES" "NO"))
        (tell-line "Strength" strength)
        (tell-line "Has stiletto" (if (= stiletto-loc :thief) "YES" (str "NO (at " stiletto-loc ")")))
        (tell-line "Daemon" (cond
                              (nil? daemon) "NOT REGISTERED"
                              (:enabled daemon) "ENABLED"
                              :else "DISABLED"))
        (utils/tell "\nBag contents:\n")
        ((fn [gs]
           (let [items (remove #{:stiletto :large-bag} bag-contents)]
             (if (empty? items)
               (utils/tell gs "  (empty)\n")
               (reduce (fn [g obj-id]
                         (let [obj (gs/get-thing g obj-id)
                               tvalue (get obj :tvalue 0)]
                           (utils/tell g (str "  - " obj-id " (" (:desc obj "?") ")"
                                              (when (pos? tvalue) (str " [value: " tvalue "]"))
                                              "\n"))))
                       gs
                       items))))))))

;;; ---------------------------------------------------------------------------
;;; $thief locate
;;; ---------------------------------------------------------------------------

(defn cmd-thief-locate
  "Show thief's location with room details."
  [game-state _args]
  (let [thief-loc (gs/get-thing-loc-id game-state :thief)
        room (gs/get-thing game-state thief-loc)
        here (:here game-state)
        room-sacred? (gs/set-thing-flag? game-state thief-loc :sacred)
        room-contents (gs/get-contents game-state thief-loc)]
    (-> game-state
        (utils/tell (str "Thief is at: " thief-loc "\n"))
        (tell-line "Room name" (:desc room "?"))
        (tell-line "Sacred" (if room-sacred? "YES (thief shouldn't be here!)" "NO"))
        (tell-line "Same as player" (if (= thief-loc here) "YES" "NO"))
        (tell-line "Room contains" (if (empty? room-contents)
                                     "nothing"
                                     (str/join ", " (map format-keyword room-contents)))))))

;;; ---------------------------------------------------------------------------
;;; $thief bag
;;; ---------------------------------------------------------------------------

(defn cmd-thief-bag
  "Show what the thief has stolen."
  [game-state _args]
  (let [bag-contents (gs/get-contents game-state :thief)
        items (remove #{:stiletto :large-bag} bag-contents)
        total-value (reduce (fn [sum obj-id]
                              (+ sum (get (gs/get-thing game-state obj-id) :tvalue 0)))
                            0
                            items)]
    (-> game-state
        (utils/tell "=== THIEF'S BAG ===\n")
        ((fn [gs]
           (if (empty? items)
             (utils/tell gs "The thief's bag is empty.\n")
             (reduce (fn [g obj-id]
                       (let [obj (gs/get-thing g obj-id)
                             tvalue (get obj :tvalue 0)
                             invisible? (gs/set-thing-flag? g obj-id :invisible)]
                         (utils/tell g (str "  " obj-id " (" (:desc obj "?") ")"
                                            (when (pos? tvalue) (str " - value: " tvalue))
                                            (when invisible? " [hidden]")
                                            "\n"))))
                     gs
                     items))))
        (utils/tell (str "Total stolen value: " total-value "\n"))
        (utils/tell (str "Items in bag: " (count items) "\n")))))

;;; ---------------------------------------------------------------------------
;;; $thief move <room>
;;; ---------------------------------------------------------------------------

(defn cmd-thief-move
  "Move thief to a specific room."
  [game-state args]
  (if (empty? args)
    (utils/tell game-state "Usage: $thief move <room-id>\n")
    (let [room-id (keyword (first args))
          room (gs/get-thing game-state room-id)]
      (if room
        (-> game-state
            (assoc-in [:objects :thief :in] room-id)
            (utils/tell (str "Thief moved to " room-id " (" (:desc room "?") ")\n")))
        (utils/tell game-state (str "Unknown room: " room-id "\n"))))))

;;; ---------------------------------------------------------------------------
;;; $thief reveal
;;; ---------------------------------------------------------------------------

(defn cmd-thief-reveal
  "Make the thief visible."
  [game-state _args]
  (-> game-state
      (gs/unset-thing-flag :thief :invisible)
      (assoc :thief-here true)
      (utils/tell "Thief is now visible.\n")))

;;; ---------------------------------------------------------------------------
;;; $thief hide
;;; ---------------------------------------------------------------------------

(defn cmd-thief-hide
  "Make the thief invisible."
  [game-state _args]
  (-> game-state
      (gs/set-thing-flag :thief :invisible)
      (assoc :thief-here false)
      (utils/tell "Thief is now invisible.\n")))

;;; ---------------------------------------------------------------------------
;;; MAIN COMMAND DISPATCH
;;; ---------------------------------------------------------------------------

(def subcommands
  {:status {:handler cmd-thief-status :help "Show comprehensive thief status"}
   :locate {:handler cmd-thief-locate :help "Where is the thief?"}
   :bag    {:handler cmd-thief-bag    :help "What has the thief stolen?"}
   :move   {:handler cmd-thief-move   :help "Move thief to a room"}
   :reveal {:handler cmd-thief-reveal :help "Make thief visible"}
   :hide   {:handler cmd-thief-hide   :help "Make thief invisible"}})

(defn cmd-thief
  "Handle $thief command."
  [game-state args]
  (if (empty? args)
    ;; Default: show status
    (cmd-thief-status game-state args)
    ;; Dispatch to subcommand
    (let [subcmd (keyword (first args))
          sub-args (rest args)
          sub-info (get subcommands subcmd)]
      (if sub-info
        ((:handler sub-info) game-state sub-args)
        ;; Unknown - show help
        (reduce (fn [gs [name info]]
                  (utils/tell gs (str "  $thief " (format-keyword name) " - " (:help info) "\n")))
                (utils/tell game-state "$thief subcommands:\n")
                (sort-by first subcommands))))))
