(defpackage :silica/impl
  (:use :cl :iter)
  (:export :silica-inductive-value))

(in-package :silica/impl)

;; When generating silica code, some of it cannot be compiled directly into lisp
;; structs. For example, data-structures and pattern-matching. This class
;; contains definitions (structs/classes) which can be used to compile these
;; things.


(defclass silica-inductive-value ()
  ((name
    :type symbol
    :reader name
    :initarg :name)
   (values
    :type array
    :reader induct-values
    :initarg :values)))

(defmethod print-object ((ivalue silica-inductive-value) stream)
  (if (induct-values ivalue)
      (progn
        (format stream "(~A" (name ivalue))
        (iter (for val in-vector (induct-values ivalue))
          (format stream " ~A" val))
        (write-string  ")" stream))
      (format stream "~A" (name ivalue))))
