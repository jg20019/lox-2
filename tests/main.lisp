(defpackage lox/tests/main
  (:use :cl
        :lox
        :rove))
(in-package :lox/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :lox)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
