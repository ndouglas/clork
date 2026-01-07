(ns clork.debug.inspect
  "Deep inspection of objects and rooms with focus on flag resolution.

   The flag system has two layers:
   1. Static :flags set - defined in object/room definitions
   2. Runtime overrides - set via set-flag/unset-flag as separate keys

   This can cause bugs when code checks (:flags obj) instead of using
   gs/set-thing-flag? which checks both sources.

   The $inspect command shows both layers clearly to aid debugging."
  (:require [clork.utils :as utils]
            [clork.game-state :as gs]
            [clojure.string :as str]))

;;; ---------------------------------------------------------------------------
;;; HELPERS
;;; ---------------------------------------------------------------------------

(defn- format-keyword
  "Format a keyword for display."
  [kw]
  (if (keyword? kw) (name kw) (str kw)))

(defn- tell-line
  "Output a line with key-value formatting."
  [game-state label value]
  (utils/tell game-state (str "  " label ": " value "\n")))

(defn- parse-thing-id
  "Parse a thing ID from string argument."
  [s]
  (when s
    (if (str/starts-with? s ":")
      (keyword (subs s 1))
      (keyword s))))

;;; ---------------------------------------------------------------------------
;;; FLAG ANALYSIS
;;; ---------------------------------------------------------------------------

;; Common flags that might be modified at runtime
(def ^:private common-flags
  #{:on :lit :open :touch :fight :tryst :staggered :dead
    :take :drop :invisible :seen :lock :unlock})

(defn- get-static-flags
  "Get the :flags set from an object/room definition."
  [thing]
  (or (:flags thing) #{}))

(defn- get-runtime-overrides
  "Get runtime flag overrides (explicit true/false keys) from thing."
  [thing]
  (reduce (fn [m k]
            (let [v (get thing k)]
              (if (or (true? v) (false? v))
                (assoc m k v)
                m)))
          {}
          (keys thing)))

(defn- get-effective-flag
  "Get the effective value of a flag, checking runtime overrides first."
  [game-state entity-type entity-id flag]
  (gs/flag? game-state entity-type entity-id flag))

(defn- analyze-flags
  "Analyze all flags on a thing, showing static, runtime, and effective values.
   Returns a map of flag -> {:static :runtime :effective}"
  [game-state entity-type entity-id thing]
  (let [static-flags (get-static-flags thing)
        runtime-overrides (get-runtime-overrides thing)
        ;; Collect all flags that appear anywhere
        all-flags (into (set (keys runtime-overrides))
                        (concat static-flags common-flags))]
    (reduce (fn [m flag]
              (let [in-static? (contains? static-flags flag)
                    runtime-val (get runtime-overrides flag)
                    effective (get-effective-flag game-state entity-type entity-id flag)]
                (assoc m flag {:static in-static?
                               :runtime runtime-val
                               :effective effective})))
            {}
            all-flags)))

(defn- format-flag-analysis
  "Format flag analysis for display."
  [flag-map]
  (let [interesting-flags (filter (fn [[_ v]]
                                    (or (:static v)
                                        (some? (:runtime v))
                                        (:effective v)))
                                  flag-map)
        sorted-flags (sort-by (comp name first) interesting-flags)]
    (if (empty? sorted-flags)
      "  (no flags set)\n"
      (str/join ""
                (map (fn [[flag {:keys [static runtime effective]}]]
                       (let [mismatch? (and (some? runtime)
                                            (not= runtime static))]
                         (str "  " (format-keyword flag) ": "
                              "effective=" effective
                              (when static " [in :flags]")
                              (when (some? runtime)
                                (str " [runtime=" runtime "]"))
                              (when mismatch? " ***OVERRIDE***")
                              "\n")))
                     sorted-flags)))))

;;; ---------------------------------------------------------------------------
;;; $inspect COMMAND
;;; ---------------------------------------------------------------------------

(defn- inspect-object
  "Inspect an object in detail."
  [game-state obj-id]
  (let [obj (get-in game-state [:objects obj-id])]
    (if obj
      (let [flag-analysis (analyze-flags game-state :objects obj-id obj)
        location (:in obj)
        loc-desc (when location
                   (if-let [loc-thing (gs/get-thing game-state location)]
                     (str " (" (:desc loc-thing "?") ")")
                     ""))]
        (-> game-state
            (utils/tell (str "=== OBJECT: " obj-id " ===\n"))
            (tell-line "desc" (:desc obj "?"))
            (tell-line "location" (str location loc-desc))
            (utils/tell "\n")
            (utils/tell "Static :flags set:\n")
            (utils/tell (str "  #{" (str/join " " (map format-keyword (sort (get-static-flags obj)))) "}\n"))
            (utils/tell "\n")
            (utils/tell "Runtime overrides:\n")
            (utils/tell (let [overrides (get-runtime-overrides obj)]
                          (if (empty? overrides)
                            "  (none)\n"
                            (str/join "" (map (fn [[k v]]
                                                (str "  " (format-keyword k) " = " v "\n"))
                                              (sort-by (comp name first) overrides))))))
            (utils/tell "\n")
            (utils/tell "Effective flag values:\n")
            (utils/tell (format-flag-analysis flag-analysis))))
      (utils/tell game-state (str "Unknown object: " obj-id "\n")))))

(defn- inspect-room
  "Inspect a room in detail."
  [game-state room-id]
  (let [room (get-in game-state [:rooms room-id])]
    (if room
      (let [flag-analysis (analyze-flags game-state :rooms room-id room)
            contents (gs/get-contents game-state room-id)]
        (-> game-state
            (utils/tell (str "=== ROOM: " room-id " ===\n"))
            (tell-line "desc" (:desc room "?"))
            (tell-line "contents" (if (empty? contents)
                                    "(empty)"
                                    (str/join ", " (map format-keyword contents))))
            (utils/tell "\n")
            (utils/tell "Static :flags set:\n")
            (utils/tell (str "  #{" (str/join " " (map format-keyword (sort (get-static-flags room)))) "}\n"))
            (utils/tell "\n")
            (utils/tell "Runtime overrides:\n")
            (utils/tell (let [overrides (get-runtime-overrides room)]
                          (if (empty? overrides)
                            "  (none)\n"
                            (str/join "" (map (fn [[k v]]
                                                (str "  " (format-keyword k) " = " v "\n"))
                                              (sort-by (comp name first) overrides))))))
            (utils/tell "\n")
            (utils/tell "Effective flag values:\n")
            (utils/tell (format-flag-analysis flag-analysis))))
      (utils/tell game-state (str "Unknown room: " room-id "\n")))))

(defn- inspect-thing
  "Inspect either object or room."
  [game-state thing-id]
  (cond
    (get-in game-state [:objects thing-id])
    (inspect-object game-state thing-id)

    (get-in game-state [:rooms thing-id])
    (inspect-room game-state thing-id)

    :else
    (utils/tell game-state (str "Unknown object or room: " thing-id "\n"))))

(defn cmd-inspect
  "Main $inspect command.

   Usage:
     $inspect <id>     - Inspect object or room by ID
     $inspect candles  - Example: inspect the candles object
     $inspect here     - Inspect current room"
  [game-state args]
  (if (empty? args)
    (-> game-state
        (utils/tell "$inspect - Deep inspection of objects/rooms\n\n")
        (utils/tell "Usage:\n")
        (utils/tell "  $inspect <id>   - Inspect object or room by ID\n")
        (utils/tell "  $inspect here   - Inspect current room\n")
        (utils/tell "\n")
        (utils/tell "Shows the two-layer flag system:\n")
        (utils/tell "  1. Static :flags set (from definitions)\n")
        (utils/tell "  2. Runtime overrides (set by set-flag/unset-flag)\n")
        (utils/tell "  3. Effective values (what gs/set-thing-flag? returns)\n")
        (utils/tell "\n")
        (utils/tell "Flags with ***OVERRIDE*** indicate runtime changes that\n")
        (utils/tell "differ from the static definition.\n"))
    (let [arg (first args)]
      (if (= arg "here")
        (inspect-thing game-state (:here game-state))
        (inspect-thing game-state (parse-thing-id arg))))))
