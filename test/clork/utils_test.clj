(in-ns 'clork.core-test)

(deftest crlf-test
  (testing "(crlf) prints a newline"
    (is (= "\n" (with-out-str (crlf))))))
