(defpackage :syntax/typed
  (:nicknames :st)
  (:use :cl :iter :alexandria))
(in-package :syntax/typed)


;; helper classes
(defclass label ())
(defclass silica-declaration ())
(defclass silica-definition ())
(defclass match-clause ())
(defclass pattern ())


;; Terms & types
(defclass term ())
(defclass type ())

;; expressions (terms & types)
(defclass var (term type))
(defclass telescope (term type))
(defclass projection (term type))
(defclass lambda (term type))

;; expressions (terms)
(defclass abstraction (term))
(defclass inductive-ctor (term))
(defclass match (term))
(defclass conditional (term))
(defclass silica-structure (term))
(defclass native-type (term))
(defclass lisp-form (term))

;; expressions (types)
(defclass type ())
(defclass forall (type))
(defclass inductive-type (type))
(defclass inductive-def (type))
(defclass silica-signature (type))
(defclass native (type))

;; expressions (kinds)
(defclass kind (kind))
(defclass kind-type (kind))
(defclass kind-arrow (kind))
