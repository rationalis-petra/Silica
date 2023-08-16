(defpackage containers/array
  (:import-from :cl
   ;; values
   :t :nil
   ;; functions
   :make-hash-table :ftype :function :hash-table :equal
   :gethash :funcall
   ;; macros
   :setf
   ;; top-level defs/forms
   :declaim :defun :in-package
   ;; special forms
   :declare :ignore
   :lambda :if :let :multiple-value-bind :return
   :&key :&optional)
  (:use :iterate)
  (:nicknames :ar)
  (:export
   :empty
   :map
   :map-do))
(in-package :containers/array)

(defun empty () (make-array '()))

;; (defun map (func array)
;;   ())
