(ns clork.script-test
  (:require [clojure.test :refer [deftest testing is]]
            [clork.script :as script]))

(deftest parse-args-test
  (testing "default config with no args"
    (let [config (script/parse-args [])]
      (is (false? (:script-mode? config)))
      (is (false? (:strict config)))
      (is (false? (:fail-on-death config)))
      (is (false? (:fail-on-parser-error config)))
      (is (nil? (:max-turns config)))
      (is (nil? (:input-file config)))
      (is (nil? (:seed config)))
      (is (false? (:quiet config)))))

  (testing "--strict enables all failure checks"
    (let [config (script/parse-args ["--strict"])]
      (is (true? (:script-mode? config)))
      (is (true? (:strict config)))
      (is (true? (:fail-on-death config)))
      (is (true? (:fail-on-parser-error config)))))

  (testing "-s is alias for --strict"
    (let [config (script/parse-args ["-s"])]
      (is (true? (:strict config)))
      (is (true? (:fail-on-death config)))))

  (testing "--fail-on-death only sets death check"
    (let [config (script/parse-args ["--fail-on-death"])]
      (is (true? (:script-mode? config)))
      (is (true? (:fail-on-death config)))
      (is (false? (:fail-on-parser-error config)))))

  (testing "--fail-on-parser-error only sets parser check"
    (let [config (script/parse-args ["--fail-on-parser-error"])]
      (is (true? (:script-mode? config)))
      (is (false? (:fail-on-death config)))
      (is (true? (:fail-on-parser-error config)))))

  (testing "--max-turns parses number"
    (let [config (script/parse-args ["--max-turns" "100"])]
      (is (true? (:script-mode? config)))
      (is (= 100 (:max-turns config)))))

  (testing "-m is alias for --max-turns"
    (let [config (script/parse-args ["-m" "50"])]
      (is (= 50 (:max-turns config)))))

  (testing "--input sets input file"
    (let [config (script/parse-args ["--input" "commands.txt"])]
      (is (true? (:script-mode? config)))
      (is (= "commands.txt" (:input-file config)))))

  (testing "-i is alias for --input"
    (let [config (script/parse-args ["-i" "test.txt"])]
      (is (= "test.txt" (:input-file config)))))

  (testing "--quiet sets quiet mode"
    (let [config (script/parse-args ["--quiet"])]
      (is (true? (:quiet config)))))

  (testing "-q is alias for --quiet"
    (let [config (script/parse-args ["-q"])]
      (is (true? (:quiet config)))))

  (testing "combining multiple flags"
    (let [config (script/parse-args ["--fail-on-death" "--max-turns" "10" "-q"])]
      (is (true? (:script-mode? config)))
      (is (true? (:fail-on-death config)))
      (is (= 10 (:max-turns config)))
      (is (true? (:quiet config)))))

  (testing "--seed sets random seed"
    (let [config (script/parse-args ["--seed" "12345"])]
      (is (= 12345 (:seed config)))))

  (testing "combining --seed with other flags"
    (let [config (script/parse-args ["--strict" "--seed" "42" "-i" "test.txt"])]
      (is (true? (:strict config)))
      (is (= 42 (:seed config)))
      (is (= "test.txt" (:input-file config))))))

(deftest determine-exit-code-test
  (testing "success with no errors"
    (let [game-state {:deaths 0 :parser-error-count 0}
          config {:fail-on-death true :fail-on-parser-error true}]
      (is (= 0 (script/determine-exit-code game-state config)))))

  (testing "death triggers exit code 1 when fail-on-death enabled"
    (let [game-state {:deaths 1 :parser-error-count 0}
          config {:fail-on-death true :fail-on-parser-error false}]
      (is (= 1 (script/determine-exit-code game-state config)))))

  (testing "death does NOT trigger exit when fail-on-death disabled"
    (let [game-state {:deaths 1 :parser-error-count 0}
          config {:fail-on-death false :fail-on-parser-error false}]
      (is (= 0 (script/determine-exit-code game-state config)))))

  (testing "parser error triggers exit code 2 when enabled"
    (let [game-state {:deaths 0 :parser-error-count 3}
          config {:fail-on-death false :fail-on-parser-error true}]
      (is (= 2 (script/determine-exit-code game-state config)))))

  (testing "parser error does NOT trigger exit when disabled"
    (let [game-state {:deaths 0 :parser-error-count 3}
          config {:fail-on-death false :fail-on-parser-error false}]
      (is (= 0 (script/determine-exit-code game-state config)))))

  (testing "max turns exceeded triggers exit code 4"
    (let [game-state {:max-turns-exceeded true}
          config {}]
      (is (= 4 (script/determine-exit-code game-state config)))))

  (testing "config error triggers exit code 5"
    (let [game-state {}
          config {:error "some error"}]
      (is (= 5 (script/determine-exit-code game-state config)))))

  (testing "death takes priority over parser error"
    (let [game-state {:deaths 1 :parser-error-count 5}
          config {:fail-on-death true :fail-on-parser-error true}]
      (is (= 1 (script/determine-exit-code game-state config))))))

(deftest exit-codes-constants-test
  (testing "exit codes are defined correctly"
    (is (= 0 (:success script/exit-codes)))
    (is (= 1 (:death script/exit-codes)))
    (is (= 2 (:parser-error script/exit-codes)))
    (is (= 3 (:exception script/exit-codes)))
    (is (= 4 (:max-turns script/exit-codes)))
    (is (= 5 (:error script/exit-codes)))))
