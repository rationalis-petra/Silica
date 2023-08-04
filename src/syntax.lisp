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
    :type opal-type
    :reader val-type
    :initarg :val-type)))
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
    :type opal-type
    :reader var-type
    :initarg :var-type)
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
    :reader declarations
    :initarg :declarations)))
(defclass arrow (opal-type)
  ((from
    :type opal-type
    :reader from
    :initarg :from)
   (to
    :type opal-type
    :reader to
    :initarg :to)))
(defclass forall (opal-type)
  ((var
    :type symbol
    :reader var
    :initarg :var)
   (body
    :type opal-type
    :reader body
    :initarg :body)))

(defclass kind () ())
(defclass kind-type (kind) ())
(defclass kind-arrow (kind)
  ((from
    :type kind
    :reader from)
   (to
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


;; TODO: α<
(defgeneric α= (l r &optional renamings shadowed)
  (:documentation "Predicate: return true if two terms equal up to α-renaming")

  ;; Types
  (:method ((l native-type) (r native-type) &optional renamings shadowed)
    (declare (ignore renamings shadowed))
    (eq (lisp-type l) (lisp-type r)))

  (:method ((l forall) (r forall) &optional renamings shadowed)
    (α= (body l) (body r)
        (acons (var l) (var r) renamings)
        (cons (cons (var l) (car shadowed))
              (cons (var r) (cdr shadowed)))))

  (:method ((l arrow) (r arrow) &optional renamings shadowed)
    (and (α= (from l) (from r) renamings shadowed)
         (α= (to l) (to r) renamings shadowed)))

  (:method ((l var) (r var) &optional renamings shadowed)
    (let ((entry (assoc (var l) renamings)))
      (if entry
          (eq (cdr entry) (var r))
          (and (eq (member (var l) (car shadowed))
                   (member (var r) (cdr shadowed)))
               (eq (var l) (var r))))))
    

  (:method ((l signature) (r signature) &optional renamings shadowed)
    (and 
     (iter (for decl-1 in (declarations l))
       (for decl-2 in (declarations r))
       (always
        (and (eq (var decl-1) (var decl-2))
             ;; todo: shadow variable bindings?
             (α= (ann decl-1) (ann decl-2) renamings shadowed))))
     (= (length (declarations l)) (length (declarations r)))))

  (:method (l r &optional renamings shadowed)
    (declare (ignore l r renamings shadowed))
    nil))



;; TODO: document/layout engine (truncate at n chars)
(defgeneric show (val)
  (:documentation "Pseudo pretty-print method")

  (:method ((val var))
    (string (var val)))

  (:method ((val opal-literal))
    (format nil "~A" (val val)))

  (:method ((val opal-struct))
    (let ((stream (make-string-output-stream)))
      (write-string "(σ" stream )
      (iter (for entry in (entries val))
        (format stream " (~A ≜ ~A)" (var entry) (show (val entry))))
      (write-string ")" stream)
      (get-output-stream-string stream))))


(defmethod print-object ((term term) stream)
  (format stream "#<term ~A>" (show term)))

(defmethod print-object ((type opal-type) stream))

(defmethod print-object ((kind kind) stream))

