(uiop:define-package lox
  (:use #:cl)
  (:import-from :lox.errors
		:*had-error*
		:*had-runtime-error*
		:lox-error
		:report)
  (:import-from :lox.scanner
		:new-scanner
		:scan-tokens)
  (:import-from :lox.parser
		:new-parser
		:parse)
  (:import-from :lox.interpreter
		:new-interpreter
		:interpret))

(in-package #:lox)

(defun main (args)
  (cond ((= (length args) 1)
	 (format t "Usage: clox [script]~%"))
	((= (length args) 1)
	 (run-file (first args)))
	(t
	 (run-prompt))))

(defun run-file (path)
  (run (uiop:read-file-string path))
  (when (or *had-error* *had-runtime-error*) :error))

(defun run-prompt ()
  (format t "> ")
  (let ((line (read-line *standard-input* nil nil)))
    (when line
      (run line))))

(defparameter *interpreter* (new-interpreter))

(defun run (source)
  (let* ((scanner (new-scanner source))
	 (tokens (scan-tokens scanner))
	 (parser (new-parser tokens))
	 (statements (parse parser)))

    ;; Stop if there was a syntax error
    (when *had-error* (return-from run))

    (interpret *interpreter* statements)))

;; run an example file
;; (run-file (asdf:system-relative-pathname :lox #p"src/examples/program1.lox") )
