(ns clork.flag-lint-test
  "Lint tests to catch flag-checking bugs.

   The flag system has two layers:
   1. Static :flags set - defined in object/room definitions
   2. Runtime overrides - set via set-flag/unset-flag as separate keys

   Using (contains? flags :xxx) only checks the static :flags set, which
   causes bugs when flags are modified at runtime.

   This test scans the codebase for potentially buggy patterns."
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]))

;;; ---------------------------------------------------------------------------
;;; KNOWN ISSUES (to be fixed later)
;;; ---------------------------------------------------------------------------
;;; These are existing bugs we've identified but haven't fixed yet.
;;; New issues will cause test failures; these are allowlisted temporarily.

(def known-issues
  "Set of [file line] pairs for known flag-checking issues.
   These should be fixed, but we allowlist them to prevent regressions."
  #{;; verbs_movement.clj - checking :open on doors
    ["src/clork/verbs_movement.clj" 366]
    ["src/clork/verbs_movement.clj" 368]
    ;; verbs_containers.clj - checking :open and :invisible
    ["src/clork/verbs_containers.clj" 66]
    ["src/clork/verbs_containers.clj" 117]
    ["src/clork/verbs_containers.clj" 129]
    ["src/clork/verbs_containers.clj" 134]
    ["src/clork/verbs_containers.clj" 251]})

;;; ---------------------------------------------------------------------------
;;; RUNTIME FLAGS
;;; ---------------------------------------------------------------------------
;;; These flags are commonly modified at runtime and should be checked via
;;; gs/set-thing-flag? rather than (contains? flags :xxx)

(def runtime-flags
  "Flags that can change during gameplay and need proper game-state checks."
  #{:on        ;; Light sources on/off
    :lit       ;; Room lighting
    :open      ;; Containers/doors open/closed
    :touch     ;; Object has been touched (affects fdesc)
    :fight     ;; Actor in combat
    :dead      ;; Actor/NPC is dead
    :seen      ;; Room has been visited
    :staggered ;; Combat state
    :tryst     ;; Thief state
    :lock      ;; Locked state (can be unlocked)
    :invisible ;; Visibility state
    })

;;; ---------------------------------------------------------------------------
;;; FILE SCANNING
;;; ---------------------------------------------------------------------------

(defn- find-clj-files
  "Find all .clj files in src directory."
  []
  (let [src-dir (io/file "src")]
    (->> (file-seq src-dir)
         (filter #(.isFile %))
         (filter #(str/ends-with? (.getName %) ".clj"))
         (map #(.getPath %)))))

(defn- scan-file-for-pattern
  "Scan a file for lines matching a pattern. Returns [{:file :line :content :match}]"
  [file-path pattern]
  (let [lines (str/split-lines (slurp file-path))]
    (->> lines
         (map-indexed (fn [idx line]
                        (when-let [match (re-find pattern line)]
                          {:file file-path
                           :line (inc idx)
                           :content (str/trim line)
                           :match match})))
         (filter some?))))

;;; ---------------------------------------------------------------------------
;;; PATTERN DETECTION
;;; ---------------------------------------------------------------------------

(defn find-contains-flags-patterns
  "Find all (contains? flags :xxx) patterns in src files.
   Returns list of matches with file, line number, and content."
  []
  (let [pattern #"\(contains\?\s+flags\s+:(\w+)\)"]
    (->> (find-clj-files)
         (mapcat #(scan-file-for-pattern % pattern))
         (vec))))

(defn find-flags-set-access-patterns
  "Find patterns like (:flags obj) that extract the static flags set.
   These are potential issues if the extracted set is used for runtime flags."
  []
  (let [pattern #"\(:flags\s+\w+\)"]
    (->> (find-clj-files)
         (mapcat #(scan-file-for-pattern % pattern))
         (vec))))

(defn classify-match
  "Classify a (contains? flags :xxx) match as safe or potentially buggy."
  [{:keys [file content match] :as m}]
  (let [flag-name (if (vector? match) (second match) (second (re-find #":(\w+)" (str match))))
        flag-kw (when flag-name (keyword flag-name))
        is-runtime-flag? (contains? runtime-flags flag-kw)
        ;; ml.clj is for ML action suggestions - checking static capabilities is fine
        is-ml-file? (str/includes? file "ml.clj")
        ;; flags.clj defines the flag system itself
        is-flags-file? (str/includes? file "flags.clj")]
    (assoc m
           :flag flag-kw
           :is-runtime-flag is-runtime-flag?
           :safe? (or (not is-runtime-flag?)
                      is-ml-file?
                      is-flags-file?))))

;;; ---------------------------------------------------------------------------
;;; TESTS
;;; ---------------------------------------------------------------------------

(defn- is-known-issue?
  "Check if a match is in the known issues allowlist."
  [{:keys [file line]}]
  (contains? known-issues [file line]))

(deftest no-new-unsafe-flag-checks
  (testing "No NEW (contains? flags :xxx) for runtime flags"
    (let [matches (find-contains-flags-patterns)
          classified (map classify-match matches)
          unsafe (filter #(not (:safe? %)) classified)
          known (filter is-known-issue? unsafe)
          new-issues (remove is-known-issue? unsafe)]
      ;; Report known issues (informational)
      (when (seq known)
        (println "\n=== KNOWN FLAG ISSUES (allowlisted) ===")
        (println (str "(" (count known) " issues to fix later)\n"))
        (doseq [{:keys [file line flag]} known]
          (println (str "  " file ":" line " - :" (name flag)))))
      ;; Fail on new issues
      (when (seq new-issues)
        (println "\n=== NEW UNSAFE FLAG CHECKS FOUND ===")
        (println "These use (contains? flags :xxx) for runtime flags.")
        (println "Use gs/set-thing-flag? instead.\n")
        (doseq [{:keys [file line content flag]} new-issues]
          (println (str "  " file ":" line))
          (println (str "    Flag: " flag))
          (println (str "    Code: " content))
          (println)))
      (is (empty? new-issues)
          (str "Found " (count new-issues) " NEW unsafe flag checks. "
               "Use gs/set-thing-flag? for runtime flags.")))))

(deftest flag-lint-report
  (testing "Flag usage summary report"
    (let [matches (find-contains-flags-patterns)
          classified (map classify-match matches)
          by-flag (group-by :flag classified)
          runtime-usage (filter #(contains? runtime-flags (first %)) by-flag)]
      (println "\n=== FLAG USAGE REPORT ===")
      (println (str "Total (contains? flags :xxx) patterns: " (count matches)))
      (println (str "Runtime flags referenced: " (count runtime-usage)))
      (println "\nRuntime flags and their locations:")
      (doseq [[flag usages] (sort-by first runtime-usage)]
        (println (str "  " flag ":"))
        (doseq [{:keys [file line safe?]} usages]
          (println (str "    " file ":" line (when safe? " (safe context)")))))
      ;; This test always passes - it's just informational
      (is true))))
