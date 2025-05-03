(uiop:define-package lox.scanner
  (:use #:cl)
  (:import-from :lox.tokens :new-token)
  (:import-from :lox.errors :lox-error)
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
    (scan-token s))
  (reverse (tokens s)))

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
      (#\* (add-token s :star))
      (#\! (add-token s (if (match s #\=) :bang-equal :bang)))
      (#\= (add-token s (if (match s #\=) :equal-equal :equal)))
      (#\< (add-token s (if (match s #\=) :less-equal :equal)))
      (#\> (add-token s (if (match s #\=) :greater-equal :equal)))
      (#\/ (if (match s #\/)
	       (iterate:iterate
		 (iterate:while (and (not (at-end? s)) (char/= (peek s) #\Newline)))
		 (advance s))
	       (add-token s :slash)))
      ((#\Space #\Return #\Tab) nil)
      (#\Newline (incf (line s)))
      (otherwise (lox-error (line s) "Unexpected character.")))))

(defmethod match ((s scanner) expected)
  "Advances if current character matches expected."
  (unless (or (at-end? s) (char/= (aref (source s) (current s)) expected))
    (advance s)))

(defmethod peek ((s scanner))
  (if (at-end? s) #\Nul
      (aref (source s) (current s))))

(defmethod at-end? ((s scanner))
  (>= (current s) (length (source s))))

(defmethod add-token ((s scanner) token-type &optional literal)
  (with-slots (start current line source tokens) s 
    (let ((text (subseq source start current)))
      (push (new-token :token-type token-type
		       :lexeme text
		       :literal literal
		       :line line) tokens))))
