(defpackage :lang-extensions
  (:use :cl :iter)
  (:export
   :-> :->>
   :when-slot
   :if-slot
   :pair :fst :snd))
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

(defmacro if-slot ((var object slot-name) if-true if-false)
  `(if (slot-boundp ,object ,slot-name)
       (let ((,var (slot-value ,object ,slot-name)))
         ,if-true)
       ,if-false))

(defmacro when-slot ((var object slot-name) &rest body)
  `(when (slot-boundp ,object ,slot-name)
     (let ((,var (slot-value ,object ,slot-name)))
       ,@body)))

(defstruct (pair
            (:conc-name)
            (:constructor pair (fst snd))
            (:print-object
             (lambda (pair stream)
               (write-string "⟨" stream)
               (print-object (fst pair) stream)
               (write-string " " stream)
               (print-object (snd pair) stream)
               (write-string "⟩" stream))))

  (fst nil :read-only t)
  (snd nil :read-only t))

