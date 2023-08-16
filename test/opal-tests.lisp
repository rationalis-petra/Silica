
(defpackage :opal.tests
  (:use :cl :opal :parachute)
  (:export :typecheck :equality))

(defun entries (&rest terms)
  (mapcar (lambda (term) (mk-entry (var term) term)) terms))
