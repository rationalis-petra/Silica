
(defpackage :opal.tests
  (:use :cl :opal :parachute)
  (:export :all
   :typing
   :equality
   :parsing
   :infixify))
(in-package :opal.tests)

;; Utility functions for constructing objects for use in testing.
(defun entries (&rest terms)
  (mapcar (lambda (term) (mk-entry (opal::var term) term)) terms))


(define-test all)
