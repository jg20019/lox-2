(uiop:define-package lox.scanner
  (:use #:cl)
  (:import-from :lox.token :new-token)
  (:export :new-scanner
	   :scan-tokens))

(in-package #:lox.scanner)

(defclass scanner ()
  ((source :initarg :source
	   :reader source)
   (start :initform 0
	  :accessor start)
   (current :initform 0
	    :accessor current)
   (line :initform 1
	 :accessor line)
   (tokens :initform (list)
	   :accessor tokens)))

(defun new-scanner (source)
  (make-instance 'scanner :source source))

(defmethod scan-tokens ((s scanner))
  (iterate:iterate
    (iterate:while (not (at-end? s)))
    (setf (start s) (current s))
    (iterate:collect (scan-token s))))

(defmethod advance ((s scanner))
  (let ((ch (aref (source s) (current s))))
    (incf (current s))
    ch))

(defmethod scan-token ((s scanner))
  (let ((ch (advance s)))
    (case ch
      (#\( (add-token s :left-paren))
      (#\) (add-token s :right-paren))
      (#\{ (add-token s :left-brace))
      (#\} (add-token s :right-brace))
      (#\, (add-token s :comma))
      (#\. (add-token s :dot))
      (#\- (add-token s :minus))
      (#\+ (add-token s :plus))
      (#\; (add-token s :semicolon))
      (#\* (add-token s :star)))))

(defmethod at-end? ((s scanner))
  (>= (current s) (length (source s))))

(defmethod add-token ((s scanner) token-type &optional literal)
  (with-slots (start current line source tokens) s 
    (let ((text (subseq source start current)))
      (new-token :token-type token-type
		 :lexeme text
		 :literal literal
		 :line line))))
