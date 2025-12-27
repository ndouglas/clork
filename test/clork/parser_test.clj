(in-ns 'clork.core-test)

;;;; ============================================================================
;;;; PARSER TESTS
;;;; ============================================================================

;;; ---------------------------------------------------------------------------
;;; LEXER TESTS - tokenize, parse-number, special-word?, lexv-from-input
;;; ---------------------------------------------------------------------------

(deftest tokenize-test
  (testing "tokenize splits input on whitespace"
    (let [result (tokenize "take the lamp")]
      (is (= 3 (count result)))
      (is (= "take" (:word (first result))))
      (is (= "the" (:word (second result))))
      (is (= "lamp" (:word (nth result 2))))))

  (testing "tokenize handles empty input"
    (is (nil? (tokenize "")))
    (is (nil? (tokenize "   ")))
    (is (nil? (tokenize nil))))

  (testing "tokenize normalizes to lowercase"
    (let [result (tokenize "TAKE THE LAMP")]
      (is (= "take" (:word (first result))))
      (is (= "the" (:word (second result))))
      (is (= "lamp" (:word (nth result 2))))))

  (testing "tokenize separates punctuation"
    (let [result (tokenize "go north. look")]
      (is (= 4 (count result)))
      (is (= "go" (:word (first result))))
      (is (= "north" (:word (second result))))
      (is (= "." (:word (nth result 2))))
      (is (= "look" (:word (nth result 3))))))

  (testing "tokenize handles commas"
    (let [result (tokenize "take lamp, sword")]
      (is (= 4 (count result)))
      (is (= "," (:word (nth result 2))))))

  (testing "tokenize tracks position"
    (let [result (tokenize "take lamp")]
      (is (= 0 (:start (first result))))
      (is (= 4 (:length (first result))))
      (is (= 5 (:start (second result))))
      (is (= 4 (:length (second result)))))))

(deftest lexv-from-input-test
  (testing "lexv-from-input creates proper structure"
    (let [result (lexv-from-input "take lamp")]
      (is (= 2 (:count result)))
      (is (= "take lamp" (:raw result)))
      (is (vector? (:tokens result)))
      (is (= 2 (count (:tokens result))))))

  (testing "lexv-from-input handles empty input"
    (let [result (lexv-from-input "")]
      (is (= 0 (:count result)))
      (is (empty? (:tokens result))))))

(deftest special-word?-test
  (testing "special-word? identifies special words"
    (is (special-word? "oops" :oops))
    (is (special-word? "again" :again))
    (is (special-word? "g" :g))
    (is (special-word? "the" :the))
    (is (special-word? "a" :a))
    (is (special-word? "an" :an))
    (is (special-word? "all" :all))
    (is (special-word? "and" :and))
    (is (special-word? "," :comma))
    (is (special-word? "but" :but))
    (is (special-word? "except" :except))
    (is (special-word? "then" :then))
    (is (special-word? "." :period))
    (is (special-word? "it" :it)))

  (testing "special-word? is case-insensitive"
    (is (special-word? "OOPS" :oops))
    (is (special-word? "The" :the))
    (is (special-word? "ALL" :all)))

  (testing "special-word? returns false for non-matches"
    (is (not (special-word? "lamp" :oops)))
    (is (not (special-word? "take" :the)))
    (is (not (special-word? "" :all)))))

(deftest parse-number-test
  (testing "parse-number parses plain numbers"
    (is (= 42 (parse-number "42")))
    (is (= 0 (parse-number "0")))
    (is (= 100 (parse-number "100")))
    (is (= 10000 (parse-number "10000"))))

  (testing "parse-number rejects numbers over 10000"
    (is (nil? (parse-number "10001")))
    (is (nil? (parse-number "99999"))))

  (testing "parse-number parses time format"
    ;; 3:30 -> 3 is < 8 so becomes 15:30 = 15*60+30 = 930
    (is (= 930 (parse-number "3:30")))
    ;; 10:00 -> 10 >= 8 so stays 10:00 = 10*60+0 = 600
    (is (= 600 (parse-number "10:00")))
    ;; 12:30 = 12*60+30 = 750
    (is (= 750 (parse-number "12:30"))))

  (testing "parse-number rejects invalid times"
    (is (nil? (parse-number "25:00")))
    (is (nil? (parse-number "10:60"))))

  (testing "parse-number rejects non-numbers"
    (is (nil? (parse-number "abc")))
    (is (nil? (parse-number "")))
    (is (nil? (parse-number nil)))
    (is (nil? (parse-number "12:ab")))))

;;; ---------------------------------------------------------------------------
;;; PARSER STATE TESTS
;;; ---------------------------------------------------------------------------

(deftest initial-parser-state-test
  (testing "initial-parser-state creates all required keys"
    (let [state (initial-parser-state)]
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
    (let [state (initial-parser-state)]
      (is (every? zero? (:itbl state)))))

  (testing "initial-parser-state otbl is zeroed"
    (let [state (initial-parser-state)]
      (is (every? zero? (:otbl state))))))

(deftest parser-constants-test
  (testing "sibreaks contains expected characters"
    (is (contains? sibreaks "."))
    (is (contains? sibreaks ","))
    (is (contains? sibreaks "\"")))

  (testing "p-itbllen is correct"
    (is (= 9 p-itbllen)))

  (testing "itbl-indices has all required keys"
    (is (= 0 (:verb itbl-indices)))
    (is (= 1 (:verbn itbl-indices)))
    (is (= 2 (:prep1 itbl-indices)))
    (is (= 6 (:nc1 itbl-indices)))
    (is (= 7 (:nc1l itbl-indices)))
    (is (= 8 (:nc2 itbl-indices)))
    (is (= 9 (:nc2l itbl-indices))))

  (testing "search-bits has all required keys"
    (is (= 128 (:held search-bits)))
    (is (= 64 (:carried search-bits)))
    (is (= 32 (:in-room search-bits)))
    (is (= 16 (:on-ground search-bits)))
    (is (= 8 (:take search-bits)))
    (is (= 4 (:many search-bits)))
    (is (= 2 (:have search-bits))))

  (testing "parts-of-speech has all required keys"
    (is (= 1 (:direction parts-of-speech)))
    (is (= 2 (:verb parts-of-speech)))
    (is (= 4 (:preposition parts-of-speech)))
    (is (= 8 (:adjective parts-of-speech)))
    (is (= 16 (:object parts-of-speech)))
    (is (= 32 (:buzz-word parts-of-speech)))))

(deftest parser-init-tbl-test
  (testing "parser-init-tbl copies itbl to otbl when not in orphan mode"
    (let [game-state {:parser {:itbl [1 2 3 4 5 6 7 8 9 10]
                               :otbl (vec (repeat 10 0))
                               :oflag false}}
          result (parser-init-tbl game-state)]
      (is (= [1 2 3 4 5 6 7 8 9 10] (get-in result [:parser :otbl])))
      (is (= (vec (repeat 10 0)) (get-in result [:parser :itbl])))))

  (testing "parser-init-tbl does not copy when in orphan mode"
    (let [game-state {:parser {:itbl [1 2 3 4 5 6 7 8 9 10]
                               :otbl [0 0 0 0 0 0 0 0 0 0]
                               :oflag true}}
          result (parser-init-tbl game-state)]
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
          result (parser-init game-state)]
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
          result (parser-init game-state)]
      (is (= :troll (get-in result [:parser :owinner])))
      (is (true? (get-in result [:parser :omerged]))))))

;;; ---------------------------------------------------------------------------
;;; OBJECT UTILITIES TESTS
;;; ---------------------------------------------------------------------------

(deftest obj-found-test
  (testing "obj-found adds to empty table"
    (is (= [:lamp] (obj-found nil :lamp)))
    (is (= [:lamp] (obj-found [] :lamp))))

  (testing "obj-found appends to existing table"
    (is (= [:lamp :sword] (obj-found [:lamp] :sword)))
    (is (= [:lamp :sword :shield] (obj-found [:lamp :sword] :shield)))))

(deftest match-table-count-test
  (testing "match-table-count returns 0 for nil/empty"
    (is (= 0 (match-table-count nil)))
    (is (= 0 (match-table-count []))))

  (testing "match-table-count returns correct count"
    (is (= 1 (match-table-count [:lamp])))
    (is (= 3 (match-table-count [:lamp :sword :shield])))))

(deftest but-merge-test
  (testing "but-merge removes excluded objects"
    (is (= [:lamp :shield] (but-merge [:lamp :sword :shield] [:sword])))
    (is (= [:lamp] (but-merge [:lamp :sword :shield] [:sword :shield]))))

  (testing "but-merge handles empty exclusion list"
    (is (= [:lamp :sword] (but-merge [:lamp :sword] []))))

  (testing "but-merge handles no matches"
    (is (= [:lamp :sword] (but-merge [:lamp :sword] [:shield])))))

;;; ---------------------------------------------------------------------------
;;; VALIDATION UTILITIES TESTS
;;; ---------------------------------------------------------------------------

(deftest room?-test
  (testing "room? returns true for rooms"
    (let [game-state {:rooms {:west-of-house {:id :west-of-house}
                              :living-room {:id :living-room}}}]
      (is (room? game-state :west-of-house))
      (is (room? game-state :living-room))))

  (testing "room? returns false for non-rooms"
    (let [game-state {:rooms {:west-of-house {:id :west-of-house}}}]
      (is (not (room? game-state :lamp)))
      (is (not (room? game-state nil))))))

(deftest meta-loc-test
  (testing "meta-loc returns nil for nil object"
    (let [game-state {:rooms {} :objects {}}]
      (is (nil? (meta-loc game-state nil)))))

  (testing "meta-loc returns room for object directly in room"
    (let [game-state {:rooms {:west-of-house {:id :west-of-house}}
                      :objects {:mailbox {:id :mailbox :in :west-of-house}}}]
      (is (= :west-of-house (meta-loc game-state :mailbox)))))

  (testing "meta-loc returns room for nested object"
    (let [game-state {:rooms {:west-of-house {:id :west-of-house}}
                      :objects {:mailbox {:id :mailbox :in :west-of-house}
                                :leaflet {:id :leaflet :in :mailbox}}}]
      (is (= :west-of-house (meta-loc game-state :leaflet)))))

  (testing "meta-loc returns :global-objects for global objects"
    (let [game-state {:rooms {}
                      :objects {:sky {:id :sky :in :global-objects}}}]
      (is (= :global-objects (meta-loc game-state :sky))))))

;;; ---------------------------------------------------------------------------
;;; SYNTAX UTILITIES TESTS
;;; ---------------------------------------------------------------------------

(deftest make-syntax-test
  (testing "make-syntax creates correct structure"
    (let [syn (make-syntax 1 nil nil :lightable nil 128 nil :light)]
      (is (= 1 (:num-objects syn)))
      (is (nil? (:prep1 syn)))
      (is (nil? (:prep2 syn)))
      (is (= :lightable (:gwim1 syn)))
      (is (nil? (:gwim2 syn)))
      (is (= 128 (:loc1 syn)))
      (is (nil? (:loc2 syn)))
      (is (= :light (:action syn)))))

  (testing "make-syntax with two objects and prepositions"
    (let [syn (make-syntax 2 nil :in nil nil 128 32 :put)]
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
      (is (syntax-matches? game-state syntax))))

  (testing "syntax-matches? allows fewer objects than required (for GWIM)"
    (let [game-state {:parser {:ncn 0
                               :itbl [0 0 nil 0 nil 0 0 0 0 0]}}
          syntax {:num-objects 1 :prep1 nil :prep2 nil}]
      (is (syntax-matches? game-state syntax))))

  (testing "syntax-matches? fails with too many objects"
    (let [game-state {:parser {:ncn 2
                               :itbl [0 0 nil 0 nil 0 0 0 0 0]}}
          syntax {:num-objects 1 :prep1 nil :prep2 nil}]
      (is (not (syntax-matches? game-state syntax)))))

  (testing "syntax-matches? checks prepositions"
    (let [game-state {:parser {:ncn 1
                               :itbl [0 0 :in 0 nil 0 0 0 0 0]}}
          syntax-match {:num-objects 1 :prep1 :in :prep2 nil}
          syntax-no-match {:num-objects 1 :prep1 :on :prep2 nil}]
      (is (syntax-matches? game-state syntax-match))
      (is (not (syntax-matches? game-state syntax-no-match))))))

;;; ---------------------------------------------------------------------------
;;; CLAUSE UTILITIES TESTS
;;; ---------------------------------------------------------------------------

(deftest clause-terminator?-test
  (testing "clause-terminator? identifies terminators"
    (is (clause-terminator? nil))
    (is (clause-terminator? "."))
    (is (clause-terminator? "then")))

  (testing "clause-terminator? returns false for non-terminators"
    (is (not (clause-terminator? "lamp")))
    (is (not (clause-terminator? "the")))
    (is (not (clause-terminator? "brass")))))

(deftest in-object-list?-test
  (testing "in-object-list? checks and-flag"
    (is (in-object-list? {:and-flag true}))
    (is (not (in-object-list? {:and-flag false})))
    (is (not (in-object-list? {})))))

;;; ---------------------------------------------------------------------------
;;; LEXV ACCESS TESTS
;;; ---------------------------------------------------------------------------

(deftest lexv-get-test
  (testing "lexv-get retrieves token at index"
    (let [game-state {:parser {:lexv {:tokens [{:word "take"}
                                               {:word "lamp"}]}}}]
      (is (= {:word "take"} (lexv-get game-state 0)))
      (is (= {:word "lamp"} (lexv-get game-state 1)))
      (is (nil? (lexv-get game-state 2))))))

(deftest lexv-word-test
  (testing "lexv-word retrieves just the word string"
    (let [game-state {:parser {:lexv {:tokens [{:word "take" :start 0}
                                               {:word "lamp" :start 5}]}}}]
      (is (= "take" (lexv-word game-state 0)))
      (is (= "lamp" (lexv-word game-state 1)))
      (is (nil? (lexv-word game-state 2))))))

(deftest lexv-count-test
  (testing "lexv-count returns token count"
    (let [game-state {:parser {:lexv {:count 3}}}]
      (is (= 3 (lexv-count game-state)))))

  (testing "lexv-count returns 0 when no lexv"
    (let [game-state {:parser {}}]
      (is (= 0 (lexv-count game-state))))))

;;; ---------------------------------------------------------------------------
;;; INPUT UTILITIES TESTS
;;; ---------------------------------------------------------------------------

(deftest stuff-test
  (testing "stuff returns source unchanged (immutable copy)"
    (let [source {:tokens [{:word "test"}] :count 1}]
      (is (= source (stuff source))))))

(deftest inbuf-stuff-test
  (testing "inbuf-stuff returns source unchanged (immutable copy)"
    (let [source "test input"]
      (is (= source (inbuf-stuff source))))))
