(uiop:define-package lox.parser
  (:use #:cl #:lox.ast)
  (:import-from :lox.tokens :token-type :literal))

(in-package :lox.parser)

(defclass parser ()
  ((tokens :initarg :tokens
	   :reader tokens)
   (current :initform 0
	    :accessor current)))

(defun new-parser (&key tokens)
  (make-instance 'parser :tokens tokens))

(defmethod expression ((p parser))
  (equality p))

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
				:right right))))))

(defmethod term ((p parser))
  (let ((expr (factor p)))
    (iterate:iterate
      (iterate:while (match p :minus :plus))
      (let ((op (previous p))
	    (right (factor p)))
	(setf expr (binary-expr :left expr
				:operator op
				:right right))))))

(defmethod factor ((p parser))
  (let ((expr (unary p)))
    (iterate:iterate
      (iterate:while (match p :slash :star))
      (let ((op (previous p))
	    (right (unary p)))
	(setf expr (binary-expr :left expr
				:operator op
				:right right))))))

(defmethod unary ((p parser))
  (if (match p :bang :minus)
      (let ((operator (previous p))
	    (right (unary p)))
	(unary-expr :operator operator
		    :right right))
      (primary p)))

(defmethod primary ((p parser))
  (cond ((match p :false) (literal-expr :value :f))
	((match p :true) (literal-expr :value :t))
	((match p :nil) (literal-expr :value nil))
	((match p :number :string)
	 (literal-expr :value (literal (previous p))))
	((match p :left-paren)
	 (let ((expr (expression p)))
	   (consume p :right-paren "Expect ')' after expression")
	   (grouping-expr :expression expr)))))

(defmethod match ((p parser) &rest token-types)
  (and (some #'(lambda (token-type) (check p token-type)) token-types)
       (advance p)))

(defmethod check ((p parser) token-type)
  (unless (at-end? p)
    (eql (token-type (peek p)) token-type)))

(defmethod advance ((p parser))
  (unless (at-end? p) (incf (current p)))
  (previous p))

(defmethod at-end? ((p parser))
  (eql (token-type (peek p)) :eof))

(defmethod peek ((p parser))
  (aref (tokens p) (current p)))

(defmethod previous ((p parser))
  (aref (tokens p) (- (current p) 1)))


