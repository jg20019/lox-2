(uiop:define-package lox.interpreter
  (:use #:cl #:lox.ast))


(in-package :lox.interpreter)

(defmethod evaluate ((expr literal-expr))
  (value expr))

(defmethod evaluate ((expr grouping-expr))
  (evaulate (expression expr)))

(defmethod evaluate ((expr unary-expr))
  (let ((right (evaluate (right expr))))
    (case (lox.tokens:token-type (operator expr))
      (:bang (not (truthy? right)))
      (:minus (- right)))))

(defmethod evaluate ((expr binary-expr))
  (let ((left (evaluate (left expr)))
	(right (evaluate (right expr))))
    (case (lox.tokens:token-type (operator expr))
      (:minus (- left right))
      (:slash (/ left right))
      (:star  (* left right))
      (:plus (cond ((and (numberp left) (numberp right))
		    (+ left right))
		   ((and (stringp left) (stringp right))
		    (concatenate 'string left right))))
      (:greater (> left right))
      (:greater-equal (>= left right))
      (:less (< left right))
      (:less-equal (<= left right)))))

(defun truthy? (x)
  (cond
    ((listp x) t)			; empty list should be truthy
    ((null x) nil)			; null is false
    ((eql :true x) t)			; true is true
    ((eql :false x) nil)		; false is false
    (t t)))              ; everything else is true
