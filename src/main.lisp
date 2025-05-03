(uiop:define-package lox
  (:use #:cl)
  (:import-from :lox.errors
		:*had-error*
		:lox-error
		:report)
  (:import-from :lox.scanner
		:new-scanner
		:scan-tokens))

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
  (when *had-error* :error))

(defun run-prompt ()
  (format t "> ")
  (let ((line (read-line *standard-input* nil nil)))
    (when line
      (run line))))

(defun run (source)
  (let* ((scanner (new-scanner source))
	 (tokens (scan-tokens scanner)))
    (iterate:iterate
      (iterate:for token in tokens)
      (format t "~a~%" token))))
