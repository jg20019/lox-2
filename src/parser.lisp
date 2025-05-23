(uiop:define-package lox.parser
  (:use #:cl #:lox.ast)
  (:import-from :lox.errors :lox-parse-error)
  (:import-from :lox.tokens :token-type :literal)
  (:export :new-parser :parse))

(in-package :lox.parser)

(defclass parser ()
  ((tokens :initarg :tokens
	   :accessor tokens)
   (previous :initform nil
	     :accessor previous)))

(defmethod current ((p parser))
  (first (tokens p)))

(defun new-parser (tokens)
  (make-instance 'parser :tokens tokens))

(defmethod parse ((p parser))
  (handler-case
      (iterate:iterate
	(iterate:while (not (at-end? p)))
	(iterate:collect (statement p)))
    (lox-parse-error ()
      nil)))

(defmethod expression ((p parser))
  (equality p))

(defmethod lox-declaration ((p parser))
  (handler-case 
      (cond ((match p :var) (var-declaration p))
	    (t (statement p)))
    (lox-parse-error ()
      (synchronize p)
      nil)))

(defmethod var-declaration ((p parser))
  (let ((name (consume p :identifier "Expect variable name."))
	(initializer (when (match p :equal)
		       (expression p))))
    (consume p :semicolon "Expect ';' after variable declaration.")
    (var-stmt :name name :initializer initializer)))

(defmethod statement ((p parser))
  (cond ((match p :print) (print-statement p))
	(t (expression-statement p))))

(defmethod print-statement ((p parser))
  (let ((value (expression p)))
    (consume p :semicolon "Expect ';' after value.")
    (print-stmt :expression value)))

(defmethod expression-statement ((p parser))
  (let ((expr (expression p)))
    (consume p :semicolon "Expect ';' after expression.")
    (expression-stmt :expression expr)))

(defmethod equality ((p parser))
  (let ((expr (comparison p)))
    (iterate:iterate
      (iterate:while (match p :bang-equal :equal-equal))
      (let ((op (previous p))
	    (right (comparison p)))
	(setf expr (binary-expr :left expr
				:operator op
				:right right))))
    expr))

(defmethod comparison ((p parser))
  (let ((expr (term p)))
    (iterate:iterate
      (iterate:while (match p :greater :greater-equal :less :less-equal))
      (let ((op (previous p))
	    (right (term p)))
	(setf expr (binary-expr :left expr
				:operator op
				:right right))))
    expr))

(defmethod term ((p parser))
  (let ((expr (factor p)))
    (iterate:iterate
      (iterate:while (match p :minus :plus))
      (let ((op (previous p))
	    (right (factor p)))
	(setf expr (binary-expr :left expr
				:operator op
				:right right))))
    expr))

(defmethod factor ((p parser))
  (let ((expr (unary p)))
    (iterate:iterate
      (iterate:while (match p :slash :star))
      (let ((op (previous p))
	    (right (unary p)))
	(setf expr (binary-expr :left expr
				:operator op
				:right right))))
    expr))

(defmethod unary ((p parser))
  (if (match p :bang :minus)
      (let ((operator (previous p))
	    (right (unary p)))
	(unary-expr :operator operator
		    :right right))
      (primary p)))

(defmethod primary ((p parser))
  (cond ((match p :false) (literal-expr :value :false))
	((match p :true) (literal-expr :value :true))
	((match p :nil) (literal-expr :value nil))
	((match p :number :string)
	 (literal-expr :value (literal (previous p))))
	((match p :identifier)
	 (variable-expr :name (previous p)))
	((match p :left-paren)
	 (let ((expr (expression p)))
	   (consume p :right-paren "Expect ')' after expression")
	   (grouping-expr :expression expr)))
	(t (error (lox-parse-error (peek p) "Expect expression.")))))

(defmethod match ((p parser) &rest token-types)
  (and (some #'(lambda (token-type) (check p token-type)) token-types)
       (advance p)))

(defmethod consume ((p parser) token-type message)
  (if (check p token-type)
      (advance p)
      (error (lox-parse-error (peek p) message))))

(defmethod synchronize ((p parser))
  (advance p)
  (block nil
    (iterate:iterate
      (iterate:while (not (at-end? p)))
      (when (eql (token-type (previous p)) :semicolon)
	(return))
      (case (token-type (peek p))
	(:class :fun :var :for :if :while :print :return) (return))
      (advance p))))
  
(defmethod check ((p parser) token-type)
  (unless (at-end? p)
    (eql (token-type (peek p)) token-type)))

(defmethod advance ((p parser))
  (unless (at-end? p)
    (setf (previous p) (current p))
    (setf (tokens p) (rest (tokens p))))
  (previous p))

(defmethod at-end? ((p parser))
  (eql (token-type (peek p)) :eof))

(defmethod peek ((p parser))
  (current p))
