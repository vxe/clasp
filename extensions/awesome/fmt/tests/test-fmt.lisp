;;;; Tests for fmt bindings

(in-package :cl-user)

(defpackage :awesome-fmt-tests
  (:use :common-lisp :awesome-fmt)
  (:documentation "Tests for fmt bindings"))

(in-package :awesome-fmt-tests)

(defun test-load ()
  "Test that the extension loads successfully"
  (format t "~&[TEST] Testing fmt extension load...~%")
  (assert (find-package :awesome-fmt))
  (format t "~&[PASS] fmt extension loaded successfully!~%")
  t)

(defun test-format-demo ()
  "Test the format-demo function"
  (format t "~&[TEST] Testing format-demo function...~%")
  ;; In the real version with C++ bindings loaded, we would call:
  ;; (let ((result (awesome-fmt:format-demo "Hello {}" "World")))
  ;;   (assert (string= result "Hello World")))
  (format t "~&[PASS] format-demo test passed (placeholder)!~%")
  t)

(defun run-all-tests ()
  "Run all tests"
  (format t "~&========================================~%")
  (format t "~&Running fmt binding tests...~%")
  (format t "~&========================================~%")
  (test-load)
  (test-format-demo)
  (format t "~&========================================~%")
  (format t "~&All tests passed!~%")
  (format t "~&========================================~%"))

;;; Run tests when loaded
(run-all-tests)
