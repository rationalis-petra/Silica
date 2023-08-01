(in-package :opal)

;; Opal programming langugae
;; Compiles to common lisp
;; FFI via common lisp
;; 

(defclass module ()
  ((imports)
   (exports)))

(defclass term () ())
(defclass struct (term)
  ())
(defclass opl-lambda (term)
  ((var
    :type symbol)
   (type
    :type type)
   (body
    :type term)))
(defclass quantify (term)
  ((var
    :type symbol)
   (body
    :type term)))

(defclass opl-type () ())
(defclass signature (type)
  ((defs
    :type vector)))
(defclass arrow (type)
  ((from
    :type type)
   (to
    :type type)))
(defclass forall (type)
  ((var
    :type symbol)
   (body
    :type type)))



;; bidirectional typechecking

(defgeneric check (term type context))
(defgeneric infer (term context))

;; evaluation via β-reduction
(defgeneric reify (term environment))
