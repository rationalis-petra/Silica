(cl:defpackage containers/hash-table
  (:import-from :cl
   ;; values
   :t :nil
   ;; types
   :ftype :function :null
   ;; functions
   :make-hash-table :hash-table :equal
   :gethash :funcall :list :car :cdr :cons
   ;; macros
   :setf
   ;; top-level defs/forms
   :declaim :defun :defmacro
   ;; special forms
   :declare :ignore
   :lambda :if :let :multiple-value-bind :return
   :&key :&optional :&rest)
  (:use :iterate)
  (:nicknames :ht)
  (:export
   :empty
   :map
   :each
   :merge
   :from-alist
   :make))

(cl:in-package :containers/hash-table)

(declaim (ftype (function () hash-table) empty))
(defun empty () (make-hash-table :test #'equal))


;; (declaim (ftype (function
;;                  (hash-table hash-table &optional &key (merge-func t))
;;                  hash-table)
;;                 merge))
(defun merge (table-1 table-2 &optional &key (by (lambda (x y) (declare (ignore y)) x)))
  (let ((table (empty)))
    (iter (for (key val) in-hashtable table-1)
      (setf (gethash key table) val))

    (iter (for (key val) in-hashtable table-2)
      (multiple-value-bind (val-2 present) (gethash key table)
        (if present
          (setf (gethash key table)
                (funcall by val-2 val))
          (setf (gethash key table) val))))
    table))


(declaim (ftype (function ((function (t) t) hash-table) hash-table)))
(defun map (func table)
  (iter
    (with table = (empty))
    (for (key val) in-hashtable table)

    (setf (gethash key table) (funcall func val))

    (finally (return table))))

(declaim (ftype (function ((function (t) t) hash-table) null) each))
(defun each (func table)
  (iter
    (for (key val) in-hashtable table)
    (funcall func val)))


(defun from-alist (alist)
  (iter (for (key . val) in alist)
    (with table = (empty))

    (setf (gethash key table) val)

    (finally (return table))))

(defmacro make (&rest entries)
  `(from-alist
    (list
     ,@(iter (for entry in entries)
         (collect `(cons ,(car entry) ,(cdr entry)))))))
