(defpackage containers/alist
  (:import-from :cl
   ;; values
   :t :nil
   ;; functions
   :funcall :apply :cdr :car :cons :assoc :acons
   ;; types
   :ftype :function :null :list
   ;; list functions (to lift)
   :mapcar :append
   ;; macros
   :setf
   ;; top-level defs/forms
   :declaim :defun :in-package
   ;; special forms
   :declare :ignore
   :lambda :if :let :multiple-value-bind :return
   :&key :&optional :&rest)
  (:use :iterate)
  (:nicknames :al)
  (:export
   :empty :lookup :map :each :insert
   :<>))
(in-package :containers/alist)


(defun empty () ())

(defun lookup (key alist)
  (cdr (assoc key alist)))

(defun insert (key val alist)
  (acons key val alist))

(defun map (func list)
  (iter (for el in list)
    (collect (cons (car el) (funcall func (cdr el))))))

(defun each (func list)
  (iter (for x in list) (funcall func (cdr x))))

(defun <> (&rest args) (apply #'append args))
