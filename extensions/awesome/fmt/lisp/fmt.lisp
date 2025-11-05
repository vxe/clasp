;;;; Lisp interface for fmt bindings
;;;; This is an example demonstrating the structure

(defpackage :awesome-fmt
  (:use :common-lisp)
  (:nicknames :fmt)
  (:documentation "Bindings for the fmt C++ formatting library")
  (:export
   #:format-demo
   #:format-error))

(in-package :awesome-fmt)

(defun load-bindings ()
  "Load the fmt extension bindings.
   In a real implementation, this would load the compiled shared library."
  (format t "~&Loading fmt bindings (example)...~%")
  ;; Note: Actual loading would be done via clasp's foreign function interface
  ;; For now, this is just a placeholder to demonstrate structure
  t)

;; Initialize
(load-bindings)

(format t "~&Awesome-fmt package loaded!~%")
