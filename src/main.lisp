(uiop:define-package lox
  (:use #:cl)
  (:import-from :lox.scanner
		:new-scanner
		:scan-tokens))

(in-package #:lox)

(defparameter *had-error* nil "Is true if error has occured")

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

;; Error Handling

(defun lox-error (line message)
  (report line "" message))

(defun report (line where message)
  (format
   *error-output*
   "[line ~a] Error ~a: ~a" line where message)
  (finish-output)
  (setf *had-error* t))
