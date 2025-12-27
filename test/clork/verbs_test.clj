(in-ns 'clork.core-test)

;;; ---------------------------------------------------------------------------
;;; VERB HANDLER TESTS
;;; ---------------------------------------------------------------------------

(deftest v-verbose-test
  (testing "v-verbose sets verbose to true and super-brief to false"
    (let [[output state] (with-captured-output (v-verbose (initial-game-state)))]
      (is (= true (:verbose state)))
      (is (= false (:super-brief state)))
      (is (= "Maximum verbosity." output)))))

(deftest v-brief-test
  (testing "v-brief sets verbose and super-brief to false"
    (let [[output state] (with-captured-output (v-brief (initial-game-state)))]
      (is (= false (:verbose state)))
      (is (= false (:super-brief state)))
      (is (= "Brief descriptions." output)))))

(deftest v-super-brief-test
  (testing "v-super-brief sets super-brief to true"
    (let [[output state] (with-captured-output (v-super-brief (initial-game-state)))]
      (is (= true (:super-brief state)))
      (is (= "Superbrief descriptions." output)))))

;;; ---------------------------------------------------------------------------
;;; VERB REGISTRATION TESTS (vocabulary + syntax + dispatch)
;;; ---------------------------------------------------------------------------

(deftest verb-vocabulary-test
  (testing "verbose is registered in vocabulary as a verb"
    (is (= true (wt? "verbose" :verb)))
    (is (= :verbose (wt? "verbose" :verb true))))
  (testing "brief is registered in vocabulary as a verb"
    (is (= true (wt? "brief" :verb)))
    (is (= :brief (wt? "brief" :verb true))))
  (testing "superbrief is registered in vocabulary as a verb"
    (is (= true (wt? "superbrief" :verb)))
    (is (= :super-brief (wt? "superbrief" :verb true))))
  (testing "version is registered in vocabulary as a verb"
    (is (= true (wt? "version" :verb)))
    (is (= :version (wt? "version" :verb true)))))

(deftest verb-syntax-test
  (testing "verbose has a 0-argument syntax entry"
    (let [syntaxes (get verb-syntaxes :verbose)]
      (is (= 1 (count syntaxes)))
      (is (= 0 (:num-objects (first syntaxes))))
      (is (= :verbose (:action (first syntaxes))))))
  (testing "brief has a 0-argument syntax entry"
    (let [syntaxes (get verb-syntaxes :brief)]
      (is (= 1 (count syntaxes)))
      (is (= :brief (:action (first syntaxes)))))))

(deftest verb-parsing-test
  (testing "parsing 'verbose' sets prsa to :verbose"
    (let [gs (make-test-state)
          [_ result] (with-captured-output (parse-test-input gs "verbose"))]
      (is (nil? (get-in result [:parser :error])))
      (is (= :verbose (get-in result [:parser :prsa])))))
  (testing "parsing 'brief' sets prsa to :brief"
    (let [gs (make-test-state)
          [_ result] (with-captured-output (parse-test-input gs "brief"))]
      (is (nil? (get-in result [:parser :error])))
      (is (= :brief (get-in result [:parser :prsa]))))))

(deftest verb-dispatch-test
  (testing "perform dispatches :verbose to v-verbose"
    (let [gs (-> (make-test-state)
                 (assoc-in [:parser :prsa] :verbose))
          [output result] (with-captured-output (perform gs))]
      (is (= "Maximum verbosity." output))
      (is (= true (:verbose result)))))
  (testing "perform dispatches :brief to v-brief"
    (let [gs (-> (make-test-state)
                 (assoc-in [:parser :prsa] :brief))
          [output result] (with-captured-output (perform gs))]
      (is (= "Brief descriptions." output)))))
