(defpackage lox/tests/scanner
  (:use :cl
	:lox.scanner
	:rove)
  (:import-from :lox.tokens
		:token-type
		:literal
		:line
		:lexeme))

(in-package :lox/tests/scanner)

(deftest test-scanner
  (testing "should handle operators"
    (let* ((s (new-scanner "// this is a comment
(( )){} // grouping stuff
!*+-/=<> <= == // operators
"))
	   (tokens (scan-tokens s)))
      (ok (= 16 (length tokens)))))

  (testing "should scan string"
    (let* ((s (new-scanner "\"hello\""))
	   (tokens (scan-tokens s)))
      (ok (= (length tokens) 1))
      (ok (string= (lexeme (first tokens)) "\"hello\""))
      (ok (string= (literal (first tokens)) "hello"))
      (ok (eql :string (token-type (first tokens))))))

  (testing "should scan numbers"
    (let* ((s (new-scanner "123"))
	   (tokens (scan-tokens s)))
      (ok (= (length tokens) 1))
      (ok (= (literal (first tokens)) 123))))

  (testing "should scan floats"
    (let* ((s (new-scanner "123.40"))
	   (tokens (scan-tokens s)))
      (ok (= (length tokens) 1))
      (ok (= (literal (first tokens)) 123.4))))

  (testing "should not include trailing decimals when scanning numbers"
    (let* ((s (new-scanner "123."))
	   (tokens (scan-tokens s)))
      (ok (= (length tokens) 2))))

  (testing "should scan identifiers"
    (let* ((s (new-scanner "hello"))
	   (tokens (scan-tokens s)))
      (ok (= (length tokens) 1))
      (ok (equal (token-type (first tokens)) :identifier))
      (ok (null (literal (first tokens))))
      (ok (string= (lexeme (first tokens)) "hello"))))

  (testing "should scan identifiers with underscores"
    (let* ((s (new-scanner "_hel_lo"))
	   (tokens (scan-tokens s)))
      (ok (= (length tokens) 1))
      (ok (equal (token-type (first tokens)) :identifier))))

  (testing "should scan identifiers with numbers"
    (let* ((s (new-scanner "_hello123"))
	   (tokens (scan-tokens s)))
      (ok (= (length tokens) 1))
      (ok (equal (token-type (first tokens)) :identifier))))

  (testing "should recognize identifiers"
    (let* ((s (new-scanner "or"))
	   (tokens (scan-tokens s)))
      (ok (= (length tokens) 1))
      (ok (equal (token-type (first tokens)) :or)))))
