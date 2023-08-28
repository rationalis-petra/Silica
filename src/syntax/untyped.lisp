(defpackage syntax/untyped
  (:nicknames :ut)
  (:use :cl :iter)
  (:export
   :term))

(defclass ast ()
  ((source)))

;; helper classes
(defclass label (ast))
(defclass silica-declaration (ast))
(defclass silica-definition (ast))
(defclass match-clause (ast))
(defclass pattern (ast))

;; Root 
(defclass term (ast))

;; expressions (types or values)
(defclass variable (term))
(defclass telescope (term))
(defclass projection (term))
(defclass silica-lambda (term))

;; expressions (values)
(defclass abstraction (term))
(defclass inductive-ctor (term))
(defclass match (term))
(defclass conditional (term))
(defclass silica-structure (term))
(defclass native-type (term))
(defclass lisp-form (term))

;; expressions (types)
(defclass forall (term))
(defclass inductive-type (term))
(defclass inductive-def (term))
(defclass silica-signature (term))
(defclass native (term))

;; expressions (kinds or types)
(defclass arrow (term))

;; expressions (kinds)
(defclass kind-type (term))
