(ns clork.flags-test
  (:require [clojure.test :refer :all]
            [clork.core :refer :all]))

(deftest get-default-flags-test
  (testing "(get-default-flags) returns an empty set"
    (is (= #{} (get-default-flags)))))
