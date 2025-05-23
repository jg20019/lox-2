(uiop:define-package lox.scanner
  (:use #:cl)
  (:import-from :lox.tokens :new-token)
  (:import-from :lox.errors :lox-error)
  (:import-from :serapeum :dict)
  (:export :new-scanner
	   :scan-tokens))

(in-package #:lox.scanner)

(defparameter *keywords*
  (dict
   "and"    :and
   "class"  :class
   "else"   :else
   "false"  :false
   "for"    :for
   "fun"    :fun
   "if"     :if
   "nil"    :nil
   "or"     :or
   "print"  :print
   "return" :return
   "super"  :super
   "this"   :this
   "true"   :true
   "var"    :var
   "while"  :while))

(serapeum:toggle-pretty-print-hash-table t)

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
  (reverse (cons (new-token :token-type :eof
			    :lexeme ""
			    :literal nil
			    :line (line s))
		 (tokens s))))

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
      (#\< (add-token s (if (match s #\=) :less-equal :less)))
      (#\> (add-token s (if (match s #\=) :greater-equal :greater)))
      (#\/ (if (match s #\/)
	       (iterate:iterate
		 (iterate:while (and (not (at-end? s)) (char/= (peek s) #\Newline)))
		 (advance s))
	       (add-token s :slash)))
      ((#\Space #\Return #\Tab) nil)
      (#\Newline (incf (line s)))
      (#\" (scan-string s))
      (otherwise
       (cond ((digit-char-p ch) (scan-number s))
	     ((alpha? ch) (scan-identifier s))
	     (t (lox-error (line s) "Unexpected character.")))))))


(defmethod scan-identifier ((s scanner))
  (iterate:iterate
    (iterate:while (alpha-numeric? (peek s)))
    (advance s))

  (with-slots (source start current) s
    (let* ((text (subseq source start current))
	   (token-type (gethash text *keywords* :identifier)))
      (add-token s token-type))))

(defmethod scan-number ((s scanner))
  (iterate:iterate
    (iterate:while (digit-char-p (peek s)))
    (advance s))

  ;; Look for a fractional part
  (when (and (char= (peek s) #\.) (digit-char-p (peek-next s)))
    (advance s)				; consume the "."
    (iterate:iterate
      (iterate:while (digit-char-p (peek s)))
      (advance s)))

  (with-slots (source current start) s
    (add-token
     s
     :number
     (serapeum:parse-float (subseq source start current)))))

(defmethod scan-string ((s scanner))
  (iterate:iterate
    (iterate:while (and (char/= (peek s) #\") (not (at-end? s))))
    (when (char= #\Newline (peek s))
      (incf (line s)))
    (advance s))

  (when (at-end? s)
    (lox-error (line s) "Unterminated string.")
    (return-from scan-string))

  (advance s)				; the closing "

  (let ((value (subseq (source s) (+ (start s) 1) (- (current s) 1))))
    (add-token s :string value)))

(defmethod match ((s scanner) expected)
  "Advances if current character matches expected."
  (unless (or (at-end? s) (char/= (aref (source s) (current s)) expected))
    (advance s)))

(defmethod peek ((s scanner))
  (if (at-end? s) #\Nul
      (aref (source s) (current s))))

(defmethod peek-next ((s scanner))
  (with-slots (source current) s
    (if (>= (+ current 1) (length source))
	#\Nul
	(aref source (+ current 1)))))

(defun alpha? (ch)
  (or (alpha-char-p ch) (char= ch #\_)))

(defun alpha-numeric? (ch)
  (or (alpha? ch) (digit-char-p ch)))

(defmethod at-end? ((s scanner))
  (>= (current s) (length (source s))))

(defmethod add-token ((s scanner) token-type &optional literal)
  (with-slots (start current line source tokens) s 
    (let ((text (subseq source start current)))
      (push (new-token :token-type token-type
		       :lexeme text
		       :literal literal
		       :line line) tokens))))
