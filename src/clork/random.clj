(ns clork.random
  "Seeded random number generation for reproducible game execution.

   In normal play, randomness adds variety. In testing/CI, we want
   reproducible results, so we allow setting a seed.

   Usage:
     (init! 12345)        ; Set seed for reproducibility
     (init!)              ; Use random seed (normal play)
     (rand-int* 10)       ; Random int 0-9
     (rand-nth* coll)     ; Random element from collection
     (rand*)              ; Random float 0.0-1.0

   Speculative Execution:
     (save-state)         ; Save current seed + call count
     (restore-state! s)   ; Restore to saved state
     (clone-state)        ; Get state that can be restored later
     (with-speculative [state] ...) ; Execute speculatively")

(def ^:private rng-atom
  "Atom holding the current Random instance."
  (atom (java.util.Random.)))

(def ^:private rng-seed
  "Atom holding the seed used to initialize current RNG."
  (atom nil))

(def ^:private rng-call-count
  "Atom tracking number of RNG calls since last init."
  (atom 0))

(defn init!
  "Initialize the random number generator.
   With no args, uses a random seed (normal play).
   With a seed, enables reproducible sequences (testing)."
  ([]
   (let [seed (System/nanoTime)]
     (reset! rng-seed seed)
     (reset! rng-call-count 0)
     (reset! rng-atom (java.util.Random. seed))))
  ([seed]
   (reset! rng-seed (long seed))
   (reset! rng-call-count 0)
   (reset! rng-atom (java.util.Random. (long seed)))))

(defn get-seed-info
  "Returns info about current RNG state (for debugging)."
  []
  {:seed @rng-seed
   :call-count @rng-call-count
   :rng @rng-atom})

(defn rand*
  "Seeded replacement for rand. Returns float 0.0-1.0."
  []
  (swap! rng-call-count inc)
  (.nextDouble ^java.util.Random @rng-atom))

(defn rand-int*
  "Seeded replacement for rand-int. Returns int 0 to n-1."
  [n]
  (swap! rng-call-count inc)
  (.nextInt ^java.util.Random @rng-atom (int n)))

(defn rand-nth*
  "Seeded replacement for rand-nth. Returns random element from coll."
  [coll]
  (let [v (vec coll)]
    (nth v (rand-int* (count v)))))

(defn shuffle*
  "Seeded replacement for shuffle."
  [coll]
  (let [al (java.util.ArrayList. ^java.util.Collection (vec coll))
        n (count coll)]
    ;; shuffle makes n-1 swaps, each consuming one RNG call
    (swap! rng-call-count + (max 0 (dec n)))
    (java.util.Collections/shuffle al @rng-atom)
    (vec al)))

;;; ---------------------------------------------------------------------------
;;; SPECULATIVE EXECUTION SUPPORT
;;; ---------------------------------------------------------------------------

(defrecord RngState [seed call-count])

(defn save-state
  "Save current RNG state for later restoration.
   Returns an RngState that can be passed to restore-state!"
  []
  (->RngState @rng-seed @rng-call-count))

(defn restore-state!
  "Restore RNG to a previously saved state.
   Re-initializes with the same seed and advances to the same position."
  [^RngState state]
  (reset! rng-seed (:seed state))
  (reset! rng-call-count 0)
  (reset! rng-atom (java.util.Random. (long (:seed state))))
  ;; Advance RNG to saved position by consuming calls
  (dotimes [_ (:call-count state)]
    (.nextDouble ^java.util.Random @rng-atom))
  (reset! rng-call-count (:call-count state))
  state)

(defn clone-rng
  "Create a new Random instance at the same state as current.
   Useful for speculative execution without modifying global state."
  []
  (let [seed @rng-seed
        count @rng-call-count
        new-rng (java.util.Random. (long seed))]
    ;; Advance to same position
    (dotimes [_ count]
      (.nextDouble new-rng))
    new-rng))

(defmacro with-speculative
  "Execute body with RNG state that will be restored after.
   Returns [result final-rng-state].

   Usage:
     (with-speculative []
       (rand-int* 10)
       (rand-int* 10))

   The RNG state is restored to what it was before the block."
  [bindings & body]
  `(let [saved-state# (save-state)
         result# (do ~@body)
         final-state# (save-state)]
     (restore-state! saved-state#)
     [result# final-state#]))

(defn advance-rng!
  "Advance the RNG by n calls (consuming randomness).
   Useful for finding different combat outcomes."
  [n]
  (dotimes [_ n]
    (rand*)))
