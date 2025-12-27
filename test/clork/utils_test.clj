(in-ns 'clork.core-test)

(deftest crlf-test
  (testing "(crlf game-state) prints a newline and returns game-state"
    (let [game-state {:test true}]
      (is (= "\n" (with-out-str (crlf game-state))))
      (is (= game-state (crlf game-state))))))
