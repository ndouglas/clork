(ns clork.verbs
  "Verb handler functions - DEPRECATED.

   This file has been split into smaller modules:
   - verbs_meta.clj      - Meta commands (version, verbose, brief, inventory)
   - verbs_health.clj    - Health/scoring (diagnose, score, quit)
   - verbs_inventory.clj - Inventory manipulation (take, drop, read)
   - verbs_containers.clj - Container handling (open, examine, look-inside)
   - verbs_movement.clj  - Movement commands (walk, climb, through, move)
   - verbs_look.clj      - Look commands (look, describe-room)

   Import the specific modules you need instead of this namespace.")

;; This namespace is kept for backwards compatibility documentation only.
;; All handlers have been moved to their respective modules.
