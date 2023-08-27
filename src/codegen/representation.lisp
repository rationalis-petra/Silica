(defpackage :sigil/impl
  (:use :cl :iter)
  (:export :sigil-inductive-value))

(in-package :sigil/impl)

;; When generating sigil code, some of it cannot be compiled directly into lisp
;; structs. For example, data-structures and pattern-matching. This class
;; contains definitions (structs/classes) which can be used to compile these
;; things.


(defclass sigil-inductive-value ()
  ((name
    :type symbol
    :reader name
    :initarg :name)
   (values
    :type array
    :reader induct-values
    :initarg :values)))
