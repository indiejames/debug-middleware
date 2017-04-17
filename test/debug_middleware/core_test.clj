(ns debug-middleware.core-test
  (:require [clojure.test :refer :all]
            [debug-middleware.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))

(deftest a-test
  (testing "Boolean"
    (is (= false true))))

(deftest exception-test
  (testing "Exceptions"
    (is (= true (throw (Exception. "An exception"))))))
