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
    :initarg :form-type)
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
;; Lambda Related
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
(defclass abstract (term)
  ((var
    :type symbol
    :reader var
    :initarg :var)
   (var-kind
    :type kind
    :reader var-kind
    :initarg :var-kind)
   (body
    :type term
    :reader body
    :initarg :body)))
(defclass app (term opal-type)
  ((left
    :type term
    :reader left
    :initarg :left)
   (right
    :type (or term type)
    :reader right
    :initarg :right)))
;; Structure related
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
;; Recursive datatype-related
(defclass inductive-ctor (term)
  ((val-type
    :type opal-type
    :reader val-type
    :initarg :val-type)
   (name
    :type symbol
    :reader name
    :initarg :name)))
(defclass inductive-val (term)
  ((constructor
    :type inductive-ctor
    :reader constructor
    :initarg constructor)
   (vals
    :type (vector term)
    :reader vals
    :initarg :vals)))


(defclass opal-type () ())
(defclass native-type (opal-type)
  ((native-type
    :type t
    :reader native-type
    :initarg :native-type)))
(defclass arrow (opal-type)
  ((from
    :type opal-type
    :reader from
    :initarg :from)
   (to
    :type opal-type
    :reader to
    :initarg :to)))
(defclass tapp (opal-type)
  ((left
    :type opal-type
    :reader left
    :initarg :left)
   (right
    :type opal-type
    :reader right
    :initarg :right)))
(defclass forall (opal-type)
  ((var
    :type symbol
    :reader var
    :initarg :var)
   (var-kind
    :type kind
    :reader var-kind
    :initarg :var-kind
    :initform (mk-kind))
   (body
    :type opal-type
    :reader body
    :initarg :body)))
(defclass signature (opal-type)
  ((declarations
    :type list
    :reader declarations
    :initarg :declarations)))
(defclass inductive-type (opal-type)
  ((kind
    :type kind
    :reader kind
    :initarg :kind)
   (constructors
    :type (vector inductive-ctor)
    :reader constructors
    :initarg :constructors)))

(defclass kind () ())
(defclass kind-type (kind) ())
(defclass kind-arrow (kind)
  ((from
    :type kind
    :reader from
    :initarg :from)
   (to
    :type kind
    :reader to
    :initarg :to)))

(defclass var (term opal-type)
  ((var
    :type symbol
    :reader var
    :initarg :var)))

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

;; Declaration constructors
(defun mk-decl (var ann)
  (make-instance 'opal-declaration :var var :ann ann))
(defun mk-def (var val)
  (make-instance 'opal-definition :var var :val val))

;; kind constructorss
(defun mk-kind ()
  (make-instance 'kind-type))
(defun mk-karr (from to)
  (make-instance 'kind-arrow :from from :to to))

;; Type constructors
(defun mk-∀ (var snd &optional body)
  (if body
      (make-instance 'forall
                     :var var :var-kind snd :body body)
      (make-instance 'forall :var var :body snd)))
(defun mk-native (native-type)
  (make-instance 'native-type :native-type native-type))
(defun mk-arr (from to)
  (make-instance 'arrow :from from :to to))
(defun mk-tapp (left right)
  (make-instance 'tapp :left left :right right))
(defun mk-sig (declarations)
  (make-instance 'signature :declarations declarations))

;; Term constructors
(defun mk-lisp (type form)
  (make-instance 'lisp-form :form-type type :form form))
(defun mk-λ (var snd &optional body)
  (if body
      (make-instance 'opal-lambda
                     :var var :var-type snd :body body)
      (make-instance 'opal-lambda :var var :body snd)))
(defun mk-abs (var snd &optional body)
  (if body
      (make-instance 'abstract :var var :var-kind snd :body body)
      (make-instance 'abstract :var var :body snd)))
(defun mk-var (var)
  (make-instance 'var :var var))
(defun mk-app (left right)
  (make-instance 'app :left left :right right))
(defun mk-struct (defs)
  (make-instance 'opal-struct :entries defs)) 
(defun mk-proj (field struct)
  (make-instance 'projection :structure struct :field field))
(defun mk-val (val)
  (make-instance 'opal-literal :val val))

(defgeneric atomic? (term)
  (:method ((term var)) t)
  (:method ((term opal-literal)) t)
  (:method ((term kind-type)) t)
  (:method (term) nil))

(defgeneric get-field (term field)
  (:method ((term opal-struct) field)
    (iter (for elt in (entries term))
      (when (and (eq (var elt) field)
                 (typep elt 'opal-definition))
        (return (ann elt)))))

  (:method ((term signature) field)
    (iter (for decl in (declarations term))
      (when (eq (var decl) field)
        (return (ann decl))))))

;; TODO: α<
(defgeneric α= (l r &optional renamings shadowed)
  (:documentation "Predicate: return true if two terms equal up to α-renaming")
  ;; Terms 
  (:method ((l opal-literal) (r opal-literal) &optional renamings shadowed)
    (declare (ignore renamings shadowed))
    (equal (val l) (val r)))

  (:method ((l opal-lambda) (r opal-lambda) &optional renamings shadowed)
    (and 
     (or (and (not (slot-boundp l 'var-type)) (not (slot-boundp r 'var-type)))
         (α= (var-type l) (var-type r)))
     (α= (body l) (body r)
         (acons (var l) (var r) renamings)
         (cons (cons (var l) (car shadowed))
               (cons (var l) (car shadowed))))))

  (:method ((l abstract) (r abstract) &optional renamings shadowed)
    (and 
     (or (not (slot-boundp l 'var-kind))
         (not (slot-boundp r 'var-kind))
         (α= (var-kind l) (var-kind r)))
     (α= (body l) (body r)
         (acons (var l) (var r) renamings)
         (cons (cons (var l) (car shadowed))
               (cons (var l) (car shadowed))))))

  (:method ((l opal-struct) (r opal-struct) &optional renamings shadowed)
    (and 
     (iter (for elt-1 in (entries l))
           (for elt-2 in (entries r))
       (always
        (and (eq (var elt-1) (var elt-2))
             ;; todo: shadow variable bindings?
             (typecase (cons elt-1 elt-2) 
               ((cons opal-declaration opal-declaration)
                (α= (ann elt-1) (ann elt-2) renamings shadowed))
               ((cons opal-definition opal-definition)
                (α= (val elt-1) (val elt-2) renamings shadowed))))))
     (= (length (entries l)) (length (entries r)))))

  (:method ((l app) (r app) &optional renamings shadowed)
    (and
     (α= (left l) (left r) renamings shadowed)
     (α= (right l) (right r) renamings shadowed)))

  ;; Types
  (:method ((l native-type) (r native-type) &optional renamings shadowed)
    (declare (ignore renamings shadowed))
    (eq (native-type l) (native-type r)))

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


  ;; Kind Equality
  (:method ((k1 kind-type) (k2 kind-type) &optional renamings shadowed)
    (declare (ignore renamings shadowed))
    t)

  (:method ((k1 kind-arrow) (k2 kind-arrow) &optional renamings shadowed)
    (declare (ignore renamings shadowed))
    (and (α= (from k1) (from k2)) (α= (to k1) (to k2))))
  
  (:method (l r &optional renamings shadowed)
    (declare (ignore l r renamings shadowed))
    nil))


;; TODO: document/layout engine (truncate at n chars)
(defgeneric show (val)
  (:documentation "Pseudo pretty-print method")

  (:method ((val opal-literal))
    (format nil "~A" (val val)))

  (:method ((val lisp-form))
    (format nil "(lisp ~A ~A ~A)" (form-type val) nil (form val)))

  (:method ((val var))
    (string (var val)))

  (:method ((val app))
    (let ((stream (make-string-output-stream)))
      (flet ((mparen (val)
               (if (atomic? val)
                 (show val)
                 (concatenate 'string "(" (show val) ")"))))
        (write-string (mparen (left val)) stream)
        (write-string " " stream)
        (write-string (mparen (right val)) stream))
      (get-output-stream-string stream)))

  (:method ((val opal-lambda))
    (format nil "λ ~A. ~A" (var val) (show (body val))))

  (:method ((val abstract))
    (format nil "Λ ~A. ~A" (var val) (show (body val))))

  (:method ((val opal-struct))
    (let ((stream (make-string-output-stream)))
      (write-string "(σ" stream )
      (iter (for entry in (entries val))
        (typecase entry
          (opal-declaration
           (format stream " (~A : ~A)" (var entry) (show (ann entry))))
          (opal-definition
           (format stream " (~A ≜ ~A)" (var entry) (show (val entry))))))
      (write-string ")" stream)
      (get-output-stream-string stream)))

  (:method ((val projection))
    (let ((stream (make-string-output-stream)))
      (flet ((mparen (val)
               (if (atomic? val)
                   (show val)
                   (concatenate 'string "(" (show val) ")"))))
        (format stream "~A.~A"
                (mparen (opal-struct val))
                (field val)))))


  ;; For types
  (:method ((type native-type))
    (format nil "~A" (native-type type)))

  (:method ((type arrow))
    (format nil "~A → ~A" (show (from type)) (show (to type))))

  (:method ((type forall))
    (format nil "∀ ~A. ~A" (var type) (show (body type))))

  (:method ((type tapp))
    (let ((stream (make-string-output-stream)))
      (flet ((mparen (val)
               (if (atomic? val)
                 (show val)
                 (concatenate 'string "(" (show val) ")"))))
        (write-string (mparen (left type)) stream)
        (write-string " " stream)
        (write-string (mparen (right type)) stream))
      (get-output-stream-string stream)))

  (:method ((type signature))
    (let ((stream (make-string-output-stream)))
      (write-string "(Σ" stream )
      (iter (for entry in (declarations type))
        (format stream " (~A : ~A)" (var entry) (show (ann entry))))
      (write-string ")" stream)
      (get-output-stream-string stream)))

  ;; For Kinds
  (:method ((kind kind-type)) "τ")

  (:method ((kind kind-arrow))
    (format nil "(~A → ~A)" (show (from kind)) (show (to kind))))

  (:method ((decl opal-declaration))
    (format nil "(~A ◂ ~A)"  (var decl) (show (ann decl))))

  (:method ((defn opal-definition))
    (format nil "(~A ≜ ~A)"  (var defn) (show (val defn))))

  (:method (unknown)
    (format nil "Cannot Show object of type ~A" (type-of unknown))))


(defmethod print-object ((term term) stream)
  (format stream "#<term ~A>" (show term)))

(defmethod print-object ((type opal-type) stream)
  (format stream "#<type ~A>" (show type)))

(defmethod print-object ((kind kind) stream)
  (format stream "#<kind ~A>" (show kind)))

(defmethod print-object ((decl opal-declaration) stream)
  (format stream "#<decl ~A>" (show decl)))

(defmethod print-object ((def opal-definition) stream)
  (format stream "#<def ~A>" (show def)))

