(uiop:define-package lox.tokens
  (:use #:cl)
  (:export
   :new-token
   :token-type
   :lexeme
   :literal
   :line))

(in-package #:lox.tokens)

(defclass token ()
  ((token-type :initarg :token-type
	       :reader token-type)
   (lexeme :initarg :lexeme
	   :reader lexeme)
   (literal :initarg :literal
	    :reader literal)
   (line :initarg :line
	 :reader line)))

(defun new-token (&key token-type lexeme literal line)
  (make-instance 'token
		 :token-type token-type
		 :lexeme lexeme
		 :literal literal
		 :line line))

(defmethod to-string ((tok token))
  (with-slots (token-type lexeme literal) tok
    (format nil "~a ~a ~a" token-type lexeme literal)))

(defmethod print-object ((tok token) stream)
  (print-unreadable-object (tok stream :identity t)
    (format stream (to-string tok))))
