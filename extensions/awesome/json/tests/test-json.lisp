;;;; Tests for json bindings
(in-package :cl-user)

(defpackage :awesome-json-tests
  (:use :common-lisp :awesome-json)
  (:documentation "Tests for json bindings"))

(in-package :awesome-json-tests)

(defun test-load ()
  "Test that the extension loads successfully"
  (format t "~&Testing json extension load...~%")
  (format t "~&json extension loaded successfully!~%")
  t)

(defun run-all-tests ()
  "Run all tests"
  (format t "~&Running json tests...~%")
  (test-load)
  (format t "~&All tests passed!~%"))

;;; Run tests when loaded
(run-all-tests)
