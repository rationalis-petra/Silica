(defpackage :lang-extensions
  (:use :cl :iter)
  (:export
   :-> :->>))
(in-package :lang-extensions)

(defmacro -> (form &rest forms)
  (iter (for sexpr in forms)
    (accumulate
     sexpr
     by (lambda (sexpr inner)
          (typecase sexpr
            (cons
             (cons (car sexpr) (cons inner (cdr sexpr))))
            (t (error (format nil "Threading macro -> expects FORMS to be lists of at
  least one element, got ~A" sexpr)))))
     initial-value form)))


(defmacro ->> (form &rest forms)
  (iter (for sexpr in forms)
    (accumulate
     sexpr
     by (lambda (sexpr inner)
          (typecase sexpr
            (cons (append sexpr (list inner)))
            (t (error (format nil "Threading macro ->> expects FORMS to be lists of at
  least one element, got ~A" sexpr)))))
     initial-value form)))
