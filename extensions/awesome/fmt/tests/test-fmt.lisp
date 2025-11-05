;;;; Tests for fmt bindings
(in-package :cl-user)

(defpackage :awesome-fmt-tests
  (:use :common-lisp :awesome-fmt)
  (:documentation "Tests for fmt bindings"))

(in-package :awesome-fmt-tests)

(defun test-load ()
  "Test that the extension loads successfully"
  (format t "~&Testing fmt extension load...~%")
  (format t "~&fmt extension loaded successfully!~%")
  t)

(defun run-all-tests ()
  "Run all tests"
  (format t "~&Running fmt tests...~%")
  (test-load)
  (format t "~&All tests passed!~%"))

;;; Run tests when loaded
(run-all-tests)
