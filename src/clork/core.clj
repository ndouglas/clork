(ns clork.core
  (:gen-class)
  (:require [clojure.pprint :refer [pprint]]
            [clork.flags :as flags]
            [clork.utils :as utils]
            [clork.parser.state :as parser-state]
            [clork.game-state :as game-state]
            [clork.verbs :as verbs]
            [clork.verb-defs :as verb-defs]
            [clork.parser :as parser]
            [clork.rooms :as rooms]
            [clork.objects :as objects]
            [clork.verbs-look :as verbs-look]
            [clork.main-loop :as main-loop]))

;; Re-export clork.flags functions
(def get-default-flags flags/get-default-flags)
(def flags flags/flags)

;; Re-export clork.utils functions
(def tell utils/tell)
(def crlf utils/crlf)
(def crlf-if utils/crlf-if)
(def this-is-it utils/this-is-it)

;; Re-export clork.parser.state functions and constants
(def sibreaks parser-state/sibreaks)
(def p-itbllen parser-state/p-itbllen)
(def itbl-indices parser-state/itbl-indices)
(def syntax-offsets parser-state/syntax-offsets)
(def p-synlen parser-state/p-synlen)
(def p-sonums parser-state/p-sonums)
(def initial-parser-state parser-state/initial-parser-state)
(def parser-init-tbl parser-state/parser-init-tbl)
(def parser-init parser-state/parser-init)
(def parser-success parser-state/parser-success)
(def parser-error parser-state/parser-error)
(def parser-result? parser-state/parser-result?)

;; Re-export clork.game-state functions and constants
(def search-bits game-state/search-bits)
(def getflags game-state/getflags)
(def initial-game-state game-state/initial-game-state)
(def set-flag game-state/set-flag)
(def unset-flag game-state/unset-flag)
(def flag? game-state/flag?)
(def set-thing-flag game-state/set-thing-flag)
(def unset-thing-flag game-state/unset-thing-flag)
(def set-thing-flag? game-state/set-thing-flag?)
(def set-here-flag game-state/set-here-flag)
(def unset-here-flag game-state/unset-here-flag)
(def set-here-flag? game-state/set-here-flag?)
(def get-thing game-state/get-thing)
(def thing-name game-state/thing-name)
(def get-thing-loc-id game-state/get-thing-loc-id)
(def get-thing-location game-state/get-thing-location)
(def get-contents game-state/get-contents)
(def verbose? game-state/verbose?)
(def get-here game-state/get-here)
(def get-winner game-state/get-winner)
(def get-winner-loc game-state/get-winner-loc)
(def get-winner-loc-id game-state/get-winner-loc-id)
(def get-player game-state/get-player)
(def add-room game-state/add-room)
(def add-rooms game-state/add-rooms)
(def add-object game-state/add-object)
(def add-objects game-state/add-objects)
(def game-state-copy game-state/game-state-copy)
(def meta-location game-state/meta-location)

;; Re-export clork.verbs functions
(def v-version verbs/v-version)
(def v-verbose verbs/v-verbose)
(def v-brief verbs/v-brief)
(def v-super-brief verbs/v-super-brief)

;; Re-export clork.verb-defs functions and vars
(def verb-definitions verb-defs/verb-definitions)
(def build-vocabulary verb-defs/build-vocabulary)
(def build-syntax-entry verb-defs/build-syntax-entry)
(def build-verb-syntaxes verb-defs/build-verb-syntaxes)
(def build-verb-handlers verb-defs/build-verb-handlers)
(def ^:dynamic *verb-vocabulary* verb-defs/*verb-vocabulary*)
(def ^:dynamic *verb-syntaxes* verb-defs/*verb-syntaxes*)
(def ^:dynamic *verb-handlers* verb-defs/*verb-handlers*)
(def perform verb-defs/perform)
(def verb-syntaxes verb-defs/*verb-syntaxes*)

;; Re-export clork.parser functions
(def parser parser/parser)
(def parser-set-winner-to-player parser/parser-set-winner-to-player)
(def lexv-from-input parser/lexv-from-input)
(def tokenize parser/tokenize)
(def wt? parser/wt?)
(def special-word? parser/special-word?)
(def lexv-word parser/lexv-word)
(def lexv-count parser/lexv-count)
(def syntax-check parser/syntax-check)
(def snarf-objects parser/snarf-objects)
(def many-check parser/many-check)
(def take-check parser/take-check)
(def accessible? parser/accessible?)
(def lit? parser/lit?)
(def parser-say parser/parser-say)
(def unknown-word parser/unknown-word)
(def cant-use parser/cant-use)
(def parse-command parser/parse-command)
(def parse-tokens parser/parse-tokens)
(def handle-oops parser/handle-oops)
(def handle-again parser/handle-again)
(def clause parser/clause)
(def orphan-merge parser/orphan-merge)
(def gwim parser/gwim)
(def lexv-get parser/lexv-get)
(def number?-token parser/number?-token)
(def parse-number parser/parse-number)
(def vocabulary parser/vocabulary)
(def parts-of-speech parser/parts-of-speech)
(def parser-read-command parser/parser-read-command)
(def parser-restore-reserve parser/parser-restore-reserve)
(def parser-restore-cont parser/parser-restore-cont)
(def parser-set-here-to-winner-loc parser/parser-set-here-to-winner-loc)
(def get-object parser/get-object)
(def search-list parser/search-list)
(def obj-found parser/obj-found)
(def global-check parser/global-check)
(def match-table-count parser/match-table-count)
(def but-merge parser/but-merge)
(def clause-terminator? parser/clause-terminator?)
(def in-object-list? parser/in-object-list?)
(def inbuf-stuff parser/inbuf-stuff)
(def make-syntax parser/make-syntax)
(def meta-loc parser/meta-loc)
(def room? parser/room?)
(def stuff parser/stuff)
(def syntax-matches? parser/syntax-matches?)

;; Re-export clork.rooms
(def west-of-house rooms/west-of-house)

;; Re-export clork.objects
(def mailbox objects/mailbox)
(def adventurer objects/adventurer)

;; Re-export clork.verbs-look
(def describe-room verbs-look/describe-room)
(def describe-objects verbs-look/describe-objects)
(def v-look verbs-look/v-look)

;; Re-export clork.main-loop
(def main-loop-once main-loop/main-loop-once)
(def main-loop main-loop/main-loop)

(defn initial-version
  "Print out the version information when starting the game"
  [game-state]
  (if (set-here-flag? game-state :touch)
    game-state
    (-> game-state
        (v-version)
        (crlf))))

(defn go
  "The GO routine."
  [game-state]
  (-> game-state
    (add-rooms [
      west-of-house,
    ])
    (add-objects [
      adventurer,
      mailbox,
    ])
    (this-is-it :mailbox)
    (initial-version)
    (set-here-flag :lit)
    (v-look)
    (main-loop)))

(defn -main
  "Main function for CLORK."
  [& args]
  (go (initial-game-state)))
