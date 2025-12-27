(ns clork.parser-test
  "Parser tests for Clork."
  (:require [clojure.test :refer :all]
            [clork.parser :as parser]
            [clork.parser.state :as parser-state]
            [clork.parser.objects :as parser-objects]
            [clork.parser.validation :as validation]
            [clork.verb-defs :as verb-defs]
            [clork.game-state :as gs]))

;;;; ============================================================================
;;;; PARSER TESTS
;;;; ============================================================================

;;; ---------------------------------------------------------------------------
;;; LEXER TESTS - tokenize, parse-number, special-word?, lexv-from-input
;;; ---------------------------------------------------------------------------

(deftest tokenize-test
  (testing "tokenize splits input on whitespace"
    (let [result (parser/tokenize "take the lamp")]
      (is (= 3 (count result)))
      (is (= "take" (:word (first result))))
      (is (= "the" (:word (second result))))
      (is (= "lamp" (:word (nth result 2))))))

  (testing "tokenize handles empty input"
    (is (nil? (parser/tokenize "")))
    (is (nil? (parser/tokenize "   ")))
    (is (nil? (parser/tokenize nil))))

  (testing "tokenize normalizes to lowercase"
    (let [result (parser/tokenize "TAKE THE LAMP")]
      (is (= "take" (:word (first result))))
      (is (= "the" (:word (second result))))
      (is (= "lamp" (:word (nth result 2))))))

  (testing "tokenize separates punctuation"
    (let [result (parser/tokenize "go north. look")]
      (is (= 4 (count result)))
      (is (= "go" (:word (first result))))
      (is (= "north" (:word (second result))))
      (is (= "." (:word (nth result 2))))
      (is (= "look" (:word (nth result 3))))))

  (testing "tokenize handles commas"
    (let [result (parser/tokenize "take lamp, sword")]
      (is (= 4 (count result)))
      (is (= "," (:word (nth result 2))))))

  (testing "tokenize tracks position"
    (let [result (parser/tokenize "take lamp")]
      (is (= 0 (:start (first result))))
      (is (= 4 (:length (first result))))
      (is (= 5 (:start (second result))))
      (is (= 4 (:length (second result)))))))

(deftest lexv-from-input-test
  (testing "lexv-from-input creates proper structure"
    (let [result (parser/lexv-from-input "take lamp")]
      (is (= 2 (:count result)))
      (is (= "take lamp" (:raw result)))
      (is (vector? (:tokens result)))
      (is (= 2 (count (:tokens result))))))

  (testing "lexv-from-input handles empty input"
    (let [result (parser/lexv-from-input "")]
      (is (= 0 (:count result)))
      (is (empty? (:tokens result))))))

(deftest special-word?-test
  (testing "special-word? identifies special words"
    (is (parser/special-word? "oops" :oops))
    (is (parser/special-word? "again" :again))
    (is (parser/special-word? "g" :g))
    (is (parser/special-word? "the" :the))
    (is (parser/special-word? "a" :a))
    (is (parser/special-word? "an" :an))
    (is (parser/special-word? "all" :all))
    (is (parser/special-word? "and" :and))
    (is (parser/special-word? "," :comma))
    (is (parser/special-word? "but" :but))
    (is (parser/special-word? "except" :except))
    (is (parser/special-word? "then" :then))
    (is (parser/special-word? "." :period))
    (is (parser/special-word? "it" :it)))

  (testing "special-word? is case-insensitive"
    (is (parser/special-word? "OOPS" :oops))
    (is (parser/special-word? "The" :the))
    (is (parser/special-word? "ALL" :all)))

  (testing "special-word? returns false for non-matches"
    (is (not (parser/special-word? "lamp" :oops)))
    (is (not (parser/special-word? "take" :the)))
    (is (not (parser/special-word? "" :all)))))

(deftest parse-number-test
  (testing "parse-number parses plain numbers"
    (is (= 42 (parser/parse-number "42")))
    (is (= 0 (parser/parse-number "0")))
    (is (= 100 (parser/parse-number "100")))
    (is (= 10000 (parser/parse-number "10000"))))

  (testing "parse-number rejects numbers over 10000"
    (is (nil? (parser/parse-number "10001")))
    (is (nil? (parser/parse-number "99999"))))

  (testing "parse-number parses time format"
    ;; 3:30 -> 3 is < 8 so becomes 15:30 = 15*60+30 = 930
    (is (= 930 (parser/parse-number "3:30")))
    ;; 10:00 -> 10 >= 8 so stays 10:00 = 10*60+0 = 600
    (is (= 600 (parser/parse-number "10:00")))
    ;; 12:30 = 12*60+30 = 750
    (is (= 750 (parser/parse-number "12:30"))))

  (testing "parse-number rejects invalid times"
    (is (nil? (parser/parse-number "25:00")))
    (is (nil? (parser/parse-number "10:60"))))

  (testing "parse-number rejects non-numbers"
    (is (nil? (parser/parse-number "abc")))
    (is (nil? (parser/parse-number "")))
    (is (nil? (parser/parse-number nil)))
    (is (nil? (parser/parse-number "12:ab")))))

;;; ---------------------------------------------------------------------------
;;; PARSER STATE TESTS
;;; ---------------------------------------------------------------------------

(deftest initial-parser-state-test
  (testing "initial-parser-state creates all required keys"
    (let [state (parser-state/initial-parser-state)]
      (is (nil? (:prsa state)))
      (is (nil? (:prso state)))
      (is (nil? (:prsi state)))
      (is (nil? (:table state)))
      (is (nil? (:syntax state)))
      (is (vector? (:itbl state)))
      (is (= 10 (count (:itbl state))))
      (is (vector? (:otbl state)))
      (is (= 10 (count (:otbl state))))
      (is (= 0 (:len state)))
      (is (= 0 (:ncn state)))
      (is (false? (:quote-flag state)))
      (is (false? (:oflag state)))
      (is (false? (:merged state)))))

  (testing "initial-parser-state itbl is zeroed"
    (let [state (parser-state/initial-parser-state)]
      (is (every? zero? (:itbl state)))))

  (testing "initial-parser-state otbl is zeroed"
    (let [state (parser-state/initial-parser-state)]
      (is (every? zero? (:otbl state))))))

(deftest parser-constants-test
  (testing "sibreaks contains expected characters"
    (is (contains? parser-state/sibreaks "."))
    (is (contains? parser-state/sibreaks ","))
    (is (contains? parser-state/sibreaks "\"")))

  (testing "p-itbllen is correct"
    (is (= 9 parser-state/p-itbllen)))

  (testing "itbl-indices has all required keys"
    (is (= 0 (:verb parser-state/itbl-indices)))
    (is (= 1 (:verbn parser-state/itbl-indices)))
    (is (= 2 (:prep1 parser-state/itbl-indices)))
    (is (= 6 (:nc1 parser-state/itbl-indices)))
    (is (= 7 (:nc1l parser-state/itbl-indices)))
    (is (= 8 (:nc2 parser-state/itbl-indices)))
    (is (= 9 (:nc2l parser-state/itbl-indices))))

  (testing "search-bits has all required keys"
    (is (= 128 (:held gs/search-bits)))
    (is (= 64 (:carried gs/search-bits)))
    (is (= 32 (:in-room gs/search-bits)))
    (is (= 16 (:on-ground gs/search-bits)))
    (is (= 8 (:take gs/search-bits)))
    (is (= 4 (:many gs/search-bits)))
    (is (= 2 (:have gs/search-bits))))

  (testing "parts-of-speech has all required keys"
    (is (= 1 (:direction parser/parts-of-speech)))
    (is (= 2 (:verb parser/parts-of-speech)))
    (is (= 4 (:preposition parser/parts-of-speech)))
    (is (= 8 (:adjective parser/parts-of-speech)))
    (is (= 16 (:object parser/parts-of-speech)))
    (is (= 32 (:buzz-word parser/parts-of-speech)))))

(deftest parser-init-tbl-test
  (testing "parser-init-tbl copies itbl to otbl when not in orphan mode"
    (let [game-state {:parser {:itbl [1 2 3 4 5 6 7 8 9 10]
                               :otbl (vec (repeat 10 0))
                               :oflag false}}
          result (parser-state/parser-init-tbl game-state)]
      (is (= [1 2 3 4 5 6 7 8 9 10] (get-in result [:parser :otbl])))
      (is (= (vec (repeat 10 0)) (get-in result [:parser :itbl])))))

  (testing "parser-init-tbl does not copy when in orphan mode"
    (let [game-state {:parser {:itbl [1 2 3 4 5 6 7 8 9 10]
                               :otbl [0 0 0 0 0 0 0 0 0 0]
                               :oflag true}}
          result (parser-state/parser-init-tbl game-state)]
      (is (= [0 0 0 0 0 0 0 0 0 0] (get-in result [:parser :otbl])))
      (is (= (vec (repeat 10 0)) (get-in result [:parser :itbl]))))))

(deftest parser-init-test
  (testing "parser-init resets transient flags"
    (let [game-state {:winner :player
                      :parser {:itbl [1 2 3 4 5 6 7 8 9 10]
                               :otbl (vec (repeat 10 0))
                               :oflag false
                               :adverb :quickly
                               :merged true
                               :end-on-prep true
                               :prso [:lamp]
                               :prsi [:case]
                               :buts [:sword]}}
          result (parser/parser-init game-state)]
      (is (nil? (get-in result [:parser :adverb])))
      (is (false? (get-in result [:parser :merged])))
      (is (false? (get-in result [:parser :end-on-prep])))
      (is (nil? (get-in result [:parser :prso])))
      (is (nil? (get-in result [:parser :prsi])))
      (is (nil? (get-in result [:parser :buts])))))

  (testing "parser-init saves winner and merged state"
    (let [game-state {:winner :troll
                      :parser {:itbl (vec (repeat 10 0))
                               :otbl (vec (repeat 10 0))
                               :oflag false
                               :merged true}}
          result (parser/parser-init game-state)]
      (is (= :troll (get-in result [:parser :owinner])))
      (is (true? (get-in result [:parser :omerged]))))))

;;; ---------------------------------------------------------------------------
;;; OBJECT UTILITIES TESTS
;;; ---------------------------------------------------------------------------

(deftest obj-found-test
  (testing "obj-found adds to empty table"
    (is (= [:lamp] (parser/obj-found nil :lamp)))
    (is (= [:lamp] (parser/obj-found [] :lamp))))

  (testing "obj-found appends to existing table"
    (is (= [:lamp :sword] (parser/obj-found [:lamp] :sword)))
    (is (= [:lamp :sword :shield] (parser/obj-found [:lamp :sword] :shield)))))

(deftest match-table-count-test
  (testing "match-table-count returns 0 for nil/empty"
    (is (= 0 (parser/match-table-count nil)))
    (is (= 0 (parser/match-table-count []))))

  (testing "match-table-count returns correct count"
    (is (= 1 (parser/match-table-count [:lamp])))
    (is (= 3 (parser/match-table-count [:lamp :sword :shield])))))

(deftest but-merge-test
  (testing "but-merge removes excluded objects"
    (is (= [:lamp :shield] (parser/but-merge [:lamp :sword :shield] [:sword])))
    (is (= [:lamp] (parser/but-merge [:lamp :sword :shield] [:sword :shield]))))

  (testing "but-merge handles empty exclusion list"
    (is (= [:lamp :sword] (parser/but-merge [:lamp :sword] []))))

  (testing "but-merge handles no matches"
    (is (= [:lamp :sword] (parser/but-merge [:lamp :sword] [:shield])))))

;;; ---------------------------------------------------------------------------
;;; VALIDATION UTILITIES TESTS
;;; ---------------------------------------------------------------------------

(deftest room?-test
  (testing "room? returns true for rooms"
    (let [game-state {:rooms {:west-of-house {:id :west-of-house}
                              :living-room {:id :living-room}}}]
      (is (parser/room? game-state :west-of-house))
      (is (parser/room? game-state :living-room))))

  (testing "room? returns false for non-rooms"
    (let [game-state {:rooms {:west-of-house {:id :west-of-house}}}]
      (is (not (parser/room? game-state :lamp)))
      (is (not (parser/room? game-state nil))))))

(deftest meta-loc-test
  (testing "meta-loc returns nil for nil object"
    (let [game-state {:rooms {} :objects {}}]
      (is (nil? (parser/meta-loc game-state nil)))))

  (testing "meta-loc returns room for object directly in room"
    (let [game-state {:rooms {:west-of-house {:id :west-of-house}}
                      :objects {:mailbox {:id :mailbox :in :west-of-house}}}]
      (is (= :west-of-house (parser/meta-loc game-state :mailbox)))))

  (testing "meta-loc returns room for nested object"
    (let [game-state {:rooms {:west-of-house {:id :west-of-house}}
                      :objects {:mailbox {:id :mailbox :in :west-of-house}
                                :leaflet {:id :leaflet :in :mailbox}}}]
      (is (= :west-of-house (parser/meta-loc game-state :leaflet)))))

  (testing "meta-loc returns :global-objects for global objects"
    (let [game-state {:rooms {}
                      :objects {:sky {:id :sky :in :global-objects}}}]
      (is (= :global-objects (parser/meta-loc game-state :sky))))))

;;; ---------------------------------------------------------------------------
;;; SYNTAX UTILITIES TESTS
;;; ---------------------------------------------------------------------------

(deftest make-syntax-test
  (testing "make-syntax creates correct structure"
    (let [syn (parser/make-syntax 1 nil nil :lightable nil 128 nil :light)]
      (is (= 1 (:num-objects syn)))
      (is (nil? (:prep1 syn)))
      (is (nil? (:prep2 syn)))
      (is (= :lightable (:gwim1 syn)))
      (is (nil? (:gwim2 syn)))
      (is (= 128 (:loc1 syn)))
      (is (nil? (:loc2 syn)))
      (is (= :light (:action syn)))))

  (testing "make-syntax with two objects and prepositions"
    (let [syn (parser/make-syntax 2 nil :in nil nil 128 32 :put)]
      (is (= 2 (:num-objects syn)))
      (is (nil? (:prep1 syn)))
      (is (= :in (:prep2 syn)))
      (is (= 128 (:loc1 syn)))
      (is (= 32 (:loc2 syn)))
      (is (= :put (:action syn))))))

(deftest syntax-matches?-test
  (testing "syntax-matches? with matching pattern"
    (let [game-state {:parser {:ncn 1
                               :itbl [0 0 nil 0 nil 0 0 0 0 0]}}
          syntax {:num-objects 1 :prep1 nil :prep2 nil}]
      (is (parser/syntax-matches? game-state syntax))))

  (testing "syntax-matches? allows fewer objects than required (for GWIM)"
    (let [game-state {:parser {:ncn 0
                               :itbl [0 0 nil 0 nil 0 0 0 0 0]}}
          syntax {:num-objects 1 :prep1 nil :prep2 nil}]
      (is (parser/syntax-matches? game-state syntax))))

  (testing "syntax-matches? fails with too many objects"
    (let [game-state {:parser {:ncn 2
                               :itbl [0 0 nil 0 nil 0 0 0 0 0]}}
          syntax {:num-objects 1 :prep1 nil :prep2 nil}]
      (is (not (parser/syntax-matches? game-state syntax)))))

  (testing "syntax-matches? checks prepositions"
    (let [game-state {:parser {:ncn 1
                               :itbl [0 0 :in 0 nil 0 0 0 0 0]}}
          syntax-match {:num-objects 1 :prep1 :in :prep2 nil}
          syntax-no-match {:num-objects 1 :prep1 :on :prep2 nil}]
      (is (parser/syntax-matches? game-state syntax-match))
      (is (not (parser/syntax-matches? game-state syntax-no-match))))))

;;; ---------------------------------------------------------------------------
;;; CLAUSE UTILITIES TESTS
;;; ---------------------------------------------------------------------------

(deftest clause-terminator?-test
  (testing "clause-terminator? identifies terminators"
    (is (parser/clause-terminator? nil))
    (is (parser/clause-terminator? "."))
    (is (parser/clause-terminator? "then")))

  (testing "clause-terminator? returns false for non-terminators"
    (is (not (parser/clause-terminator? "lamp")))
    (is (not (parser/clause-terminator? "the")))
    (is (not (parser/clause-terminator? "brass")))))

(deftest in-object-list?-test
  (testing "in-object-list? checks and-flag"
    (is (parser/in-object-list? {:and-flag true}))
    (is (not (parser/in-object-list? {:and-flag false})))
    (is (not (parser/in-object-list? {})))))

;;; ---------------------------------------------------------------------------
;;; LEXV ACCESS TESTS
;;; ---------------------------------------------------------------------------

(deftest lexv-get-test
  (testing "lexv-get retrieves token at index"
    (let [game-state {:parser {:lexv {:tokens [{:word "take"}
                                               {:word "lamp"}]}}}]
      (is (= {:word "take"} (parser/lexv-get game-state 0)))
      (is (= {:word "lamp"} (parser/lexv-get game-state 1)))
      (is (nil? (parser/lexv-get game-state 2))))))

(deftest lexv-word-test
  (testing "lexv-word retrieves just the word string"
    (let [game-state {:parser {:lexv {:tokens [{:word "take" :start 0}
                                               {:word "lamp" :start 5}]}}}]
      (is (= "take" (parser/lexv-word game-state 0)))
      (is (= "lamp" (parser/lexv-word game-state 1)))
      (is (nil? (parser/lexv-word game-state 2))))))

(deftest lexv-count-test
  (testing "lexv-count returns token count"
    (let [game-state {:parser {:lexv {:count 3}}}]
      (is (= 3 (parser/lexv-count game-state)))))

  (testing "lexv-count returns 0 when no lexv"
    (let [game-state {:parser {}}]
      (is (= 0 (parser/lexv-count game-state))))))

;;; ---------------------------------------------------------------------------
;;; INPUT UTILITIES TESTS
;;; ---------------------------------------------------------------------------

(deftest stuff-test
  (testing "stuff returns source unchanged (immutable copy)"
    (let [source {:tokens [{:word "test"}] :count 1}]
      (is (= source (parser/stuff source))))))

(deftest inbuf-stuff-test
  (testing "inbuf-stuff returns source unchanged (immutable copy)"
    (let [source "test input"]
      (is (= source (parser/inbuf-stuff source))))))

;;; ---------------------------------------------------------------------------
;;; OBJECT VOCABULARY TESTS
;;; ---------------------------------------------------------------------------

(deftest build-object-vocabulary-test
  (testing "build-object-vocabulary creates entries for synonyms"
    (let [objects [{:id :mailbox :synonym ["mailbox" "box"]}]
          vocab (verb-defs/build-object-vocabulary objects)]
      (is (contains? vocab "mailbox"))
      (is (contains? vocab "box"))
      (is (contains? (:parts-of-speech (get vocab "mailbox")) :object))
      (is (= "mailbox" (:object-value (get vocab "mailbox"))))))

  (testing "build-object-vocabulary handles adjective as string"
    (let [objects [{:id :lamp :synonym ["lamp"] :adjective "brass"}]
          vocab (verb-defs/build-object-vocabulary objects)]
      (is (contains? vocab "brass"))
      (is (contains? (:parts-of-speech (get vocab "brass")) :adjective))
      (is (= "brass" (:adj-value (get vocab "brass"))))))

  (testing "build-object-vocabulary handles adjective as vector"
    (let [objects [{:id :sword :synonym ["sword"] :adjective ["rusty" "old"]}]
          vocab (verb-defs/build-object-vocabulary objects)]
      (is (contains? vocab "rusty"))
      (is (contains? vocab "old"))
      (is (contains? (:parts-of-speech (get vocab "rusty")) :adjective))))

  (testing "build-object-vocabulary handles objects without synonyms"
    (let [objects [{:id :thing}]
          vocab (verb-defs/build-object-vocabulary objects)]
      (is (empty? vocab))))

  (testing "build-object-vocabulary normalizes to lowercase"
    (let [objects [{:id :lamp :synonym ["LAMP"] :adjective "BRASS"}]
          vocab (verb-defs/build-object-vocabulary objects)]
      (is (contains? vocab "lamp"))
      (is (contains? vocab "brass"))
      (is (not (contains? vocab "LAMP")))))

  (testing "build-object-vocabulary merges parts-of-speech for same word"
    (let [objects [{:id :light :synonym ["light"]}
                   {:id :sword :synonym ["sword"] :adjective ["light"]}]
          vocab (verb-defs/build-object-vocabulary objects)]
      (is (contains? (:parts-of-speech (get vocab "light")) :object))
      (is (contains? (:parts-of-speech (get vocab "light")) :adjective)))))

(deftest this-it?-test
  (testing "this-it? matches object by synonym"
    (let [game-state {:objects {:mailbox {:id :mailbox
                                          :synonym ["mailbox" "box"]}}
                      :rooms {}
                      :parser {:nam "mailbox"}}]
      (is (parser-objects/this-it? game-state :mailbox))))

  (testing "this-it? matches object by alternate synonym"
    (let [game-state {:objects {:mailbox {:id :mailbox
                                          :synonym ["mailbox" "box"]}}
                      :rooms {}
                      :parser {:nam "box"}}]
      (is (parser-objects/this-it? game-state :mailbox))))

  (testing "this-it? rejects non-matching name"
    (let [game-state {:objects {:mailbox {:id :mailbox
                                          :synonym ["mailbox" "box"]}}
                      :rooms {}
                      :parser {:nam "lamp"}}]
      (is (not (parser-objects/this-it? game-state :mailbox)))))

  (testing "this-it? matches adjective"
    (let [game-state {:objects {:mailbox {:id :mailbox
                                          :synonym ["mailbox"]
                                          :adjective "small"}}
                      :rooms {}
                      :parser {:nam "mailbox" :adj "small"}}]
      (is (parser-objects/this-it? game-state :mailbox))))

  (testing "this-it? rejects wrong adjective"
    (let [game-state {:objects {:mailbox {:id :mailbox
                                          :synonym ["mailbox"]
                                          :adjective "small"}}
                      :rooms {}
                      :parser {:nam "mailbox" :adj "large"}}]
      (is (not (parser-objects/this-it? game-state :mailbox)))))

  (testing "this-it? is case-insensitive"
    (let [game-state {:objects {:mailbox {:id :mailbox
                                          :synonym ["MAILBOX"]
                                          :adjective "SMALL"}}
                      :rooms {}
                      :parser {:nam "mailbox" :adj "small"}}]
      (is (parser-objects/this-it? game-state :mailbox))))

  (testing "this-it? rejects invisible objects"
    (let [game-state {:objects {:mailbox {:id :mailbox
                                          :synonym ["mailbox"]
                                          :invisible true}}
                      :rooms {}
                      :parser {:nam "mailbox"}}]
      (is (not (parser-objects/this-it? game-state :mailbox)))))

  (testing "this-it? matches when no criteria specified"
    (let [game-state {:objects {:mailbox {:id :mailbox
                                          :synonym ["mailbox"]}}
                      :rooms {}
                      :parser {}}]
      (is (parser-objects/this-it? game-state :mailbox))))

  (testing "this-it? treats gwimbit=0 as 'don't filter by flag'"
    (let [game-state {:objects {:mailbox {:id :mailbox
                                          :synonym ["mailbox"]}}
                      :rooms {}
                      :parser {:nam "mailbox" :gwimbit 0}}]
      (is (parser-objects/this-it? game-state :mailbox))))

  (testing "this-it? filters by gwimbit keyword when set"
    (let [game-state {:objects {:mailbox {:id :mailbox
                                          :synonym ["mailbox"]
                                          :flags #{:on}}}
                      :rooms {}
                      :parser {:nam "mailbox" :gwimbit :on}}]
      (is (parser-objects/this-it? game-state :mailbox)))
    (let [game-state {:objects {:mailbox {:id :mailbox
                                          :synonym ["mailbox"]
                                          :flags #{}}}
                      :rooms {}
                      :parser {:nam "mailbox" :gwimbit :on}}]
      (is (not (parser-objects/this-it? game-state :mailbox))))))

(deftest register-object-vocabulary!-test
  (testing "register-object-vocabulary! makes object words recognizable via wt?"
    ;; Save original vocabulary
    (let [original-vocab verb-defs/*verb-vocabulary*]
      (try
        ;; Register test objects
        (verb-defs/register-object-vocabulary!
         {:test-lamp {:id :test-lamp :synonym ["testlamp"] :adjective "testbrass"}})
        ;; Check that wt? now recognizes the words
        (is (parser/wt? "testlamp" :object))
        (is (parser/wt? "testbrass" :adjective))
        (is (= "testlamp" (parser/wt? "testlamp" :object true)))
        (is (= "testbrass" (parser/wt? "testbrass" :adjective true)))
        (finally
          ;; Restore original vocabulary
          (alter-var-root #'verb-defs/*verb-vocabulary* (constantly original-vocab))))))

;;; ---------------------------------------------------------------------------
;;; PRSO/PRSI ACCESS TESTS
;;; ---------------------------------------------------------------------------

(deftest get-prso-test
  (testing "get-prso returns first element when prso is a vector"
    (let [game-state {:parser {:prso [:mailbox :lamp]}}]
      (is (= :mailbox (parser-state/get-prso game-state)))))

  (testing "get-prso returns single element from single-item vector"
    (let [game-state {:parser {:prso [:mailbox]}}]
      (is (= :mailbox (parser-state/get-prso game-state)))))

  (testing "get-prso returns scalar value when prso is not a vector"
    (let [game-state {:parser {:prso :mailbox}}]
      (is (= :mailbox (parser-state/get-prso game-state)))))

  (testing "get-prso returns nil when prso is nil"
    (let [game-state {:parser {:prso nil}}]
      (is (nil? (parser-state/get-prso game-state)))))

  (testing "get-prso returns nil when prso is empty vector"
    (let [game-state {:parser {:prso []}}]
      (is (nil? (parser-state/get-prso game-state))))))

(deftest get-prso-all-test
  (testing "get-prso-all returns full vector when prso is a vector"
    (let [game-state {:parser {:prso [:mailbox :lamp :sword]}}]
      (is (= [:mailbox :lamp :sword] (parser-state/get-prso-all game-state)))))

  (testing "get-prso-all wraps scalar in vector"
    (let [game-state {:parser {:prso :mailbox}}]
      (is (= [:mailbox] (parser-state/get-prso-all game-state)))))

  (testing "get-prso-all returns nil when prso is nil"
    (let [game-state {:parser {:prso nil}}]
      (is (nil? (parser-state/get-prso-all game-state)))))

  (testing "get-prso-all returns empty vector for empty vector"
    (let [game-state {:parser {:prso []}}]
      (is (= [] (parser-state/get-prso-all game-state))))))

(deftest get-prsi-test
  (testing "get-prsi returns first element when prsi is a vector"
    (let [game-state {:parser {:prsi [:chest :bag]}}]
      (is (= :chest (parser-state/get-prsi game-state)))))

  (testing "get-prsi returns scalar value when prsi is not a vector"
    (let [game-state {:parser {:prsi :chest}}]
      (is (= :chest (parser-state/get-prsi game-state)))))

  (testing "get-prsi returns nil when prsi is nil"
    (let [game-state {:parser {:prsi nil}}]
      (is (nil? (parser-state/get-prsi game-state))))))

(deftest get-prsi-all-test
  (testing "get-prsi-all returns full vector when prsi is a vector"
    (let [game-state {:parser {:prsi [:chest :bag]}}]
      (is (= [:chest :bag] (parser-state/get-prsi-all game-state)))))

  (testing "get-prsi-all wraps scalar in vector"
    (let [game-state {:parser {:prsi :chest}}]
      (is (= [:chest] (parser-state/get-prsi-all game-state)))))

  (testing "get-prsi-all returns nil when prsi is nil"
    (let [game-state {:parser {:prsi nil}}]
      (is (nil? (parser-state/get-prsi-all game-state))))))

;;; ---------------------------------------------------------------------------
;;; IT PRONOUN RESOLUTION TESTS
;;; ---------------------------------------------------------------------------

(deftest it-pronoun-snarfem-test
  (testing "snarfem resolves 'it' to object stored in :it"
    (let [game-state (-> (gs/initial-game-state)
                         (assoc :it :mailbox)
                         (assoc-in [:parser :lexv]
                                   {:tokens [{:word "it"}] :count 1})
                         (assoc-in [:parser :itbl] (vec (repeat 10 0))))]
      (let [result (parser-objects/snarfem game-state 0 1 :prso)]
        (is (:success result))
        (is (= [:mailbox] (:matches result))))))

  (testing "snarfem returns error when :it is nil"
    (let [game-state (-> (gs/initial-game-state)
                         (assoc :it nil)
                         (assoc-in [:parser :lexv]
                                   {:tokens [{:word "it"}] :count 1})
                         (assoc-in [:parser :itbl] (vec (repeat 10 0))))]
      (let [result (parser-objects/snarfem game-state 0 1 :prso)]
        (is (not (:success result)))
        (is (= :no-it-referent (get-in result [:error :type])))))))

(deftest it-pronoun-update-after-verb-test
  (testing "perform updates :it after successful verb with object"
    ;; Set up a minimal game state with a verb handler
    (let [initial-state (-> (gs/initial-game-state)
                            (assoc :it :mailbox)
                            (assoc-in [:parser :prsa] :look)
                            (assoc-in [:parser :prso] [:leaflet]))]
      ;; After perform, :it should be updated to :leaflet
      (let [result-state (verb-defs/perform initial-state)]
        (is (= :leaflet (:it result-state))))))

  (testing "perform preserves :it when verb has no object"
    (let [initial-state (-> (gs/initial-game-state)
                            (assoc :it :mailbox)
                            (assoc-in [:parser :prsa] :look)
                            (assoc-in [:parser :prso] nil))]
      (let [result-state (verb-defs/perform initial-state)]
        (is (= :mailbox (:it result-state)))))))

;;; ---------------------------------------------------------------------------
;;; AUTO-TAKE (ITAKE) TESTS
;;; ---------------------------------------------------------------------------

(deftest itake-takeable-object-test
  (testing "itake moves takeable object to player inventory"
    (let [gs (-> (gs/initial-game-state)
                 (gs/add-room {:id :west-of-house :desc "West of House"})
                 (gs/add-object {:id :leaflet
                                 :in :west-of-house
                                 :desc "leaflet"
                                 :flags #{:take}}))
          result (validation/itake gs :leaflet)]
      ;; Should return updated game-state, not true
      (is (map? result))
      ;; Object should now be in player's inventory
      (is (= :adventurer (get-in result [:objects :leaflet :in])))
      ;; Object should have :touch flag set (using set-thing-flag?)
      (is (gs/set-thing-flag? result :leaflet :touch)))))

(deftest itake-non-takeable-object-test
  (testing "itake returns true for non-takeable object"
    (let [gs (-> (gs/initial-game-state)
                 (gs/add-room {:id :west-of-house :desc "West of House"})
                 (gs/add-object {:id :mailbox
                                 :in :west-of-house
                                 :desc "mailbox"
                                 :flags #{}}))]
      ;; Should return true (failure) for non-takeable
      (is (= true (validation/itake gs :mailbox))))))

(deftest itake-check-auto-takes-object-test
  (testing "itake-check auto-takes object when syntax has :take flag"
    (let [gs (-> (gs/initial-game-state)
                 (gs/add-room {:id :west-of-house :desc "West of House"})
                 (gs/add-object {:id :leaflet
                                 :in :west-of-house
                                 :desc "leaflet"
                                 :flags #{:take :read}})
                 (assoc-in [:parser :prso] [:leaflet]))
          ;; Simulate syntax with :take in loc bits
          take-bits (:take gs/search-bits)
          result (validation/itake-check gs :prso take-bits)]
      ;; Should succeed
      (is (:success result))
      ;; Object should be in inventory in returned game-state
      (is (= :adventurer (get-in (:game-state result) [:objects :leaflet :in]))))))

(deftest itake-check-already-held-test
  (testing "itake-check succeeds without taking if already held"
    (let [gs (-> (gs/initial-game-state)
                 (gs/add-room {:id :west-of-house :desc "West of House"})
                 (gs/add-object {:id :leaflet
                                 :in :adventurer  ; Already in inventory
                                 :desc "leaflet"
                                 :flags #{:take :read}})
                 (assoc-in [:parser :prso] [:leaflet]))
          take-bits (:take gs/search-bits)
          result (validation/itake-check gs :prso take-bits)]
      ;; Should succeed
      (is (:success result))
      ;; Object should still be in inventory
      (is (= :adventurer (get-in (:game-state result) [:objects :leaflet :in]))))))

(deftest itake-check-non-takeable-fails-silently-test
  (testing "itake-check fails silently for non-takeable object"
    (let [gs (-> (gs/initial-game-state)
                 (gs/add-room {:id :west-of-house :desc "West of House"})
                 (gs/add-object {:id :mailbox
                                 :in :west-of-house
                                 :desc "mailbox"
                                 :flags #{}})  ; No :take flag
                 (assoc-in [:parser :prso] [:mailbox]))
          take-bits (:take gs/search-bits)
          result (validation/itake-check gs :prso take-bits)]
      ;; Should still succeed (silent failure for non-takeable)
      (is (:success result))
      ;; Object should NOT be moved
      (is (= :west-of-house (get-in (:game-state result) [:objects :mailbox :in])))))))
