(uiop:define-package lox.interpreter
  (:use #:cl #:lox.ast)
  (:import-from :lox.errors
		:lox-runtime-error
		:run-time-error)
  (:export :new-interpreter
	   :interpret))

(in-package :lox.interpreter)

(defclass interpreter ()
  ())

(defun new-interpreter ()
  (make-instance 'interpreter ))

(defmethod interpret ((i interpreter) expression)
  (handler-case
      (let ((value (evaluate expression)))
	(format t "~a~%" (stringify value)))
    (run-time-error (e)
      (lox-runtime-error e))))


(defmethod evaluate ((expr literal-expr))
  (value expr))

(defmethod evaluate ((expr grouping-expr))
  (evaulate (expression expr)))

(defmethod evaluate ((expr unary-expr))
  (let ((right (evaluate (right expr))))
    (case (lox.tokens:token-type (operator expr))
      (:bang (not (truthy? right)))
      (:minus
       (check-number-operand (operator expr) right)
       (- right)))))

(defun check-number-operand (operator operand)
  (unless (numberp operand)
    (error 'run-time-error :token operator :message "Operand must be a number.")))

(defun check-number-operands (operator left right)
  (unless (and (numberp left) (numberp right))
    (error 'run-time-error :token operator :message "Operands must be numbers.")))


(defmethod evaluate ((expr binary-expr))
  (let ((left (evaluate (left expr)))
	(right (evaluate (right expr))))
    (case (lox.tokens:token-type (operator expr))
      (:minus (check-number-operands (operator expr) left right) (- left right))
      (:slash (check-number-operands (operator expr) left right) (/ left right))
      (:star  (check-number-operands (operator expr) left right) (* left right))
      (:plus (cond ((and (numberp left) (numberp right))
		    (+ left right))
		   ((and (stringp left) (stringp right))
		    (concatenate 'string left right))
		   (t (error 'run-time-error
			     :token (operator expr)
			     :message "Operands must be two numbers or two strings"))))
      (:greater
       (check-number-operands (operator expr) left right)
       (lox-boolean (> left right)))
      (:greater-equal
       (check-number-operands (operator expr) left right)
       (lox-boolean (>= left right)))
      (:less
       (check-number-operands (operator expr) left right)
       (lox-boolean (< left right)))
      (:less-equal
       (check-number-operands (operator expr) left right)
       (lox-boolean (<= left right)))
      (:bang-equal
       (lox-boolean (not (equal? left right))))
      (:equal-equal
       (lox-boolean (equal? left right))))))

(defun truthy? (x)
  (cond
    ((and (consp x) (listp x)) t)   ; empty list should be truthy
    ((null x) nil)		    ; null is false
    ((eql :true x) t)		    ; true is true
    ((eql :false x) nil)	    ; false is false
    (t t)))                         ; everything else is true

(defun equal? (a b)
  (cond ((and (null a) (null b)) :true)
	((null a) :false)
	((equal a b) :true)
	(t :false)))

(defun lox-boolean (x)
  "Converts Common Lisp Boolean t / nil to 
   Lox boolean :true and :false"
  (if x :true :false))

(defun stringify (object)
  (cond ((null object) "nil")
	((numberp object)
	 (let ((text (format nil "~a" object)))
	   (if (str:ends-with? ".0" text)
	       (str:substring 0 (- (length text) 2) text)
	       text)))
	(t (format nil "~a" object))))
