(in-ns 'clork.core)

(def initial-game-state {
  :rooms {}
  :objects {}
  :i-candles 40
  :i-lantern 200
  :here :west-of-house
  :it :mailbox
  :lit false
  :adventurer :adventurer
  :winner :adventurer
  :player :adventurer
  :verbose false
  :super-brief false
  :won false
})
