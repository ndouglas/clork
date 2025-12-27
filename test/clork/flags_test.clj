(ns clork.flags-test
  (:require [clojure.test :refer :all]
            [clork.flags :as flags]))

(deftest get-default-flags-test
  (testing "(get-default-flags) returns an empty set"
    (is (= #{} (flags/get-default-flags)))))
