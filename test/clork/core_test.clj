(ns clork.core-test
  "Main test namespace for Clork - requires all test namespaces."
  (:require [clojure.test :refer :all]
            [clork.core :refer :all]
            ;; Test namespaces - requiring them runs their tests
            [clork.flags-test]
            [clork.utils-test]
            [clork.parser-test]
            [clork.verbs-test]
            [clork.game-state-test]))