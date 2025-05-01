(uiop:define-package lox.scanner
  (:use #:cl)
  (:export :new-scanner
	   :scan-tokens))

(in-package #:lox.scanner)

(defclass scanner ()
    ((source :initarg :source)))

(defun new-scanner (source)
  (make-instance 'scanner :source source))

(defmethod scan-tokens ((s scanner))
  (list "a"))
