(uiop:define-package lox.errors
  (:use #:cl)
  (:import-from :lox.tokens :line :lexeme :token-type)
  (:export
   :*had-error*
   :lox-error
   :report-parse-error
   :report

   ;; runtime errors
   :*had-runtime-error*
   :lox-runtime-error
   :run-time-error))

(in-package #:lox.errors)

(defparameter *had-error* nil)
(defparameter *had-runtime-error* nil)

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

(define-condition run-time-error (error)
  ((token :initarg :token
	  :reader token)
   (message :initarg :message
	    :reader message)))

(defmethod print-object ((condition run-time-error) stream)
  (format stream "~A" (message condition)))

(defun lox-runtime-error (e)
  (format *error-output* "~a~%[line ~a]~%" (message e) (line (token e)))
  (setf *had-runtime-error* t))
