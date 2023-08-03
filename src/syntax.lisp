(in-package :opal)


;; Modules (toplevel constructs)
(defclass module ()
  ((opl-imports)
   (opl-exports)
   (opal-struct)))

;; Terms
(defclass term () ())
(defclass lisp-form (term)
  ((form
    :type t
    :reader form
    :initarg :form)
   (form-type
    :type opal-type
    :reader form-type
    :initarg :type)
   (bound-vars
    :type list
    :reader bound-vars
    :initarg :bound-vars)))
(defclass opal-literal (term)
  ((val
    :type t
    :reader val
    :initarg :val)
   (val-type
    :type opal-type)))
(defclass opal-struct (term)
  ((entries
    :type list
    :reader entries
    :initarg :entries)))
(defclass projection (term)
  ((opal-struct
    :type term
    :reader opal-struct
    :initarg :structure)
   (field
    :type symbol
    :reader field
    :initarg :field)))
(defclass opal-lambda (term)
  ((var
    :type symbol
    :reader var
    :initarg :var)
   (var-type
    :type opal-type)
   (body
    :type term
    :reader body
    :initarg :body)))
(defclass quantify (term)
  ((var
    :type symbol
    :reader var
    :initarg :var)
   (body
    :type term
    :reader body
    :initarg :body)))
(defclass var (term)
  ((var
    :type symbol
    :reader var
    :initarg :var)))
(defclass app (term)
  ((left
    :type term
    :reader left
    :initarg :left)
   (right
    :type (or term type)
    :reader right
    :initarg :right)))


(defclass opal-type () ())
(defclass native-type (opal-type)
  ((lisp-type
    :type t
    :reader lisp-type
    :initarg :native-type)))
(defclass signature (opal-type)
  ((declarations
    :type list
    :initarg :declarations)))
(defclass arrow (opal-type)
  ((from
    :type opal-type
    :initarg :from)
   (to
    :type opal-type
    :initarg :to)))
(defclass forall (opal-type)
  ((var
    :type symbol
    :reader var
    :initarg :var)
   (body
    :type opal-type
    :reader var
    :initarg :var)))

(defclass kind () ())
(defclass kind-univ (kind) ())
(defclass kind-arr (kind)
  ((from
    :type kind
    :reader from)
   (kind-to
    :type kind
    :reader to)))


;; Untyped declarations and definitions
;; these are replaced with either type or val declarations or definitions during typechecking.
(defclass opal-declaration ()
  ((var
    :type symbol
    :reader var
    :initarg :var)
   (ann
    :type t
    :reader ann
    :initarg :ann)))
(defclass opal-definition ()
  ((var
    :type symbol
    :reader var
    :initarg :var)
   (val
    :type t
    :reader val
    :initarg :val)))

(defclass type-declaration ()
  ((var
    :type symbol
    :reader var
    :initarg :var)
   (ann
    :type opal-type
    :reader ann
    :initarg :ann)))
(defclass val-definition ()
  ((var
    :type symbol
    :reader var
    :initarg :var)
   (val
    :type term
    :reader val
    :initarg :val)))
(defclass kind-declaration ()
 ((var
   :type symbol
   :reader var
   :initarg :var)
  (ann
   :type kind
   :reader ann
   :initarg :ann)))
(defclass type-definition ()
  ((var
    :type symbol
    :reader var
    :initarg :var)
   (val
    :type opal-type
    :reader val
    :initarg :val)))
