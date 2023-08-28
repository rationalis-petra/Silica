
(defpackage :silica-tests
  (:use :cl :silica :parachute)
  (:export :all
   :typing
   :equality
   :parsing
   :infixify))
(in-package :silica-tests)

;; Utility functions for constructing objects for use in testing.
(defun entries (&rest terms)
  (mapcar (lambda (term) (mk-entry (silica::var term) term)) terms))


(define-test all)
