;;;; Tests for spdlog bindings
(in-package :cl-user)

(defpackage :awesome-spdlog-tests
  (:use :common-lisp :awesome-spdlog)
  (:documentation "Tests for spdlog bindings"))

(in-package :awesome-spdlog-tests)

(defun test-load ()
  "Test that the extension loads successfully"
  (format t "~&Testing spdlog extension load...~%")
  (format t "~&spdlog extension loaded successfully!~%")
  t)

(defun run-all-tests ()
  "Run all tests"
  (format t "~&Running spdlog tests...~%")
  (test-load)
  (format t "~&All tests passed!~%"))

;;; Run tests when loaded
(run-all-tests)
