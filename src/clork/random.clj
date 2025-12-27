(ns clork.random
  "Seeded random number generation for reproducible game execution.

   In normal play, randomness adds variety. In testing/CI, we want
   reproducible results, so we allow setting a seed.

   Usage:
     (init! 12345)        ; Set seed for reproducibility
     (init!)              ; Use random seed (normal play)
     (rand-int* 10)       ; Random int 0-9
     (rand-nth* coll)     ; Random element from collection
     (rand*)              ; Random float 0.0-1.0")

(def ^:private rng-atom
  "Atom holding the current Random instance."
  (atom (java.util.Random.)))

(defn init!
  "Initialize the random number generator.
   With no args, uses a random seed (normal play).
   With a seed, enables reproducible sequences (testing)."
  ([]
   (reset! rng-atom (java.util.Random.)))
  ([seed]
   (reset! rng-atom (java.util.Random. (long seed)))))

(defn get-seed-info
  "Returns info about current RNG state (for debugging)."
  []
  {:seeded? true
   :rng @rng-atom})

(defn rand*
  "Seeded replacement for rand. Returns float 0.0-1.0."
  []
  (.nextDouble ^java.util.Random @rng-atom))

(defn rand-int*
  "Seeded replacement for rand-int. Returns int 0 to n-1."
  [n]
  (.nextInt ^java.util.Random @rng-atom (int n)))

(defn rand-nth*
  "Seeded replacement for rand-nth. Returns random element from coll."
  [coll]
  (let [v (vec coll)]
    (nth v (rand-int* (count v)))))

(defn shuffle*
  "Seeded replacement for shuffle."
  [coll]
  (let [al (java.util.ArrayList. ^java.util.Collection (vec coll))]
    (java.util.Collections/shuffle al @rng-atom)
    (vec al)))
