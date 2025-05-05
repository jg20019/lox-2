(uiop:define-package lox.ast
  (:use #:cl))

(in-package #:lox.ast)

(defun create-accessors (properties)
  (mapcar #'(lambda (prop)
	      `(,prop :initarg ,(alexandria:make-keyword prop)
		      :accessor ,prop))
	  properties))

(defun create-class (base-class class-def)
  (let* ((class-name (first class-def))
	 (properties (rest class-def))
	 (derived-class-name (alexandria:format-symbol *package* "~a-~a" class-name base-class)))
    `(defclass ,derived-class-name (,base-class)
      ,@(mapcar #'create-accessors properties))))

(defun create-constructor (base-class class-def)
  (let* ((class-name (first class-def))
	 (properties (second class-def))
	 (constructor-name (alexandria:format-symbol *package* "~a-~a" class-name base-class)))
    `(defun ,constructor-name (&key ,@properties)
       (make-instance ',constructor-name ,@(apply #'append
						  (mapcar #'(lambda (p)
							      (list (alexandria:make-keyword p) p))
							  properties))))))

(defmacro define-ast (base-class &body derived-classes)
  `(progn
     (defclass ,base-class () ())
     ,@(mapcar #'(lambda (derived-class) (create-class base-class derived-class)) derived-classes)
     ,@(mapcar #'(lambda (derived-class) (create-constructor base-class derived-class)) derived-classes)))

(define-ast expr
  (binary (left operator right))
  (grouping (expression))
  (literal (value))
  (unary (operator right)))

