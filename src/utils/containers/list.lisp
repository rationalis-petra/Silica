(defpackage containers/list
  (:import-from :cl
   ;; values
   :t :nil
   ;; functions
   :funcall :apply :not :or :subseq
   ;; types
   :ftype :function :null :list :integer :number
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
  (:nicknames :li)
  (:export
   :empty :iota
   :map :each
   :<> :join
   :take :drop))
(in-package :containers/list)

(declaim (ftype (function (integer &optional (function (integer) t)) list) iota))
(defun iota (num &optional (func (lambda (x) x)))
  (iter (for i from 1 to num)
    (collect (funcall func i))))

(declaim (ftype (function () list) empty))
(defun empty () nil)

(declaim (ftype (function (t list) list) map))
(defun map (func list)
  (iter (for el in list)
    (collect (funcall func el))))

(declaim (ftype (function (t list) null) each))
(defun each (func list)
  (iter (for x in list) (funcall func x)))

(declaim (ftype (function (&rest list) list) <>))
(defun <> (&rest args) (apply #'append args))

(declaim (ftype (function (list) list) join))
(defun join (list) (apply #'append list))

(declaim (ftype (function (integer list) list) take))
(defun take (n list)
  (subseq list 0 n))

(declaim (ftype (function (integer list) list) drop))
(defun drop (n list)
  (if (or (cl:= n 0) (not list))
      list
      (drop (cl:- n 1) (cl:rest list))))
