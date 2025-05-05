(uiop:define-package lox.ast
  (:use #:cl))

(in-package #:lox.ast)


(define-ast expr
  (binary (left operator right)))

(progn
  (defclass expr () ())

  (defclass binary (expr)
    ((left :initarg :left
	   :accessor left)
     (operator :initarg :operator
	       :accessor operator)

     (right :initarg :right
	    :accessor right)))

  (defun binary (&key left operator right)
    (make-instance 'binary :left left :operator operator :right right)))


(defmacro define-ast (base-class) &body derived-classes
  `(progn
     (defclass ,base-class () ())

     ,@(iterate:iterate
	 (iterate:for class-def in derived-classes)
	 ()
	 )
     ))

(macroexpand-1 '(define-ast expr))
