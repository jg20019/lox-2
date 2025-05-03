(defpackage lox/tests/scanner
  (:use :cl
	:lox.scanner
	:rove))

(in-package :lox/tests/scanner)


(deftest test-scanner
  (testing "should handle operators"
    (let* ((s (new-scanner "// this is a comment
(( )){} // grouping stuff
!*+-/=<> <= == // operators
"))
	   (tokens (scan-tokens s)))
      (ok (= 16 (length tokens))))))
