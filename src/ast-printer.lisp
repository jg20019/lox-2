(uiop:define-package lox.ast-printer
  (:use #:cl)
  (:export :print-expr))

(in-package #:lox.ast-printer)

(defun parenthesize (name &rest exprs)
  (format nil "(~a~{ ~a~})" name (mapcar #'print-expr exprs)))

(defmethod print-expr ((expr lox.ast:binary-expr))
  (with-slots (lox.ast:left lox.ast:operator lox.ast:right) expr
    (parenthesize (lox.tokens:lexeme lox.ast:operator)
		  lox.ast:left
		  lox.ast:right)))

(defmethod print-expr ((expr lox.ast:grouping-expr))
  (parenthesize "group" (lox.ast:expression expr)))

(defmethod print-expr ((expr lox.ast:literal-expr))
  (with-slots (lox.ast:value) expr
    (if (null lox.ast:value)
	"nil"
	(format nil "~a" lox.ast:value))))

(defmethod print-expr ((expr lox.ast:unary-expr))
  (with-slots (lox.ast:operator lox.ast:right) expr
    (parenthesize (lox.tokens:lexeme lox.ast:operator) lox.ast:right)))
