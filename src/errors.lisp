(uiop:define-package lox.errors
  (:use #:cl)
  (:export
   :*had-error*
   :lox-error
   :report))

(in-package #:lox.errors)

(defparameter *had-error* nil)

(defun lox-error (line message)
  (report line "" message))

(defun report (line where message)
  (format
   *error-output*
   "[line ~a] Error ~a: ~a" line where message)
  (finish-output)
  (setf *had-error* t))
