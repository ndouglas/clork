(in-ns 'clork.core-test)

(deftest get-default-flags-test
  (testing "(get-default-flags) returns an empty set"
    (is (= #{} (get-default-flags)))))
