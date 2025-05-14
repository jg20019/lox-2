(uiop:define-package lox.errors
  (:use #:cl)
  (:import-from :lox.tokens :line :lexeme :token-type)
  (:export
   :*had-error*
   :lox-error
   :report-parse-error
   :report))

(in-package #:lox.errors)

(defparameter *had-error* nil)

(defun lox-error (line message)
  (report line "" message))

(defun report-parse-error (token message)
  (with-slots (line lexeme token-type) token
    (if (eql token-type :eof)
	(report line " at end " message)
	(report line (format nil " at '~a'" lexeme) message))))

(defun report (line where message)
  (format
   *error-output*
   "[line ~a] Error ~a: ~a" line where message)
  (finish-output)
  (setf *had-error* t))
