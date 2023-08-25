
(defpackage :sigil-tests
  (:use :cl :sigil :parachute)
  (:export :all
   :typing
   :equality
   :parsing
   :infixify))
(in-package :sigil-tests)

;; Utility functions for constructing objects for use in testing.
(defun entries (&rest terms)
  (mapcar (lambda (term) (mk-entry (sigil::var term) term)) terms))


(define-test all)
