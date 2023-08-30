(in-package :silica)


;; Modules (toplevel constructs)
(defclass module ()
  ((opl-imports)
   (opl-exports)
   (silica-struct)))

;; 'untyped' terms; 
(defclass var (term silica-type)
  ((var
    :type symbol
    :reader var
    :initarg :var)))
(defclass app (term silica-type)
  ((left
    :type term
    :reader left
    :initarg :left)
   (right
    :type (or term type)
    :reader right
    :initarg :right)))
(defclass silica-lambda (term silica-type)
  ((var
    :type symbol
    :reader var
    :initarg :var)
   (var-type
    :type (or silica-type kind)
    :reader var-type
    :initarg :var-type)
   (body
    :type term
    :reader body
    :initarg :body)))

;; entry object; used in signatures/structures
(defclass entry ()
  ;; TODO: how to expand to inductive types (multiple vars/defs)
  ((var
    :type symbol
    :reader var
    :initarg :var)
   (binder
    :type (or silica-declaration silica-definition)
    :reader binder
    :initarg :binder)))

;; Terms
(defclass term () ())
(defclass term-var (term)
  ((var
    :type symbol
    :reader var
    :initarg :var)))

(defclass label (term)
  ((name
    :reader name
    :initarg :name))
  (:documentation "A label is like a symbol, but it refers to a value in an
  imported module, allowing for modules to be renamed"))
;; (defclass term-app (term silica-type)
;;   ((left
;;     :type term
;;     :reader left
;;     :initarg :left)
;;    (right
;;     :type (or term type)
;;     :reader right
;;     :initarg :right)))
(defclass lisp-form (term)
  ((form
    :type t
    :reader form
    :initarg :form)
   (form-type
    :type silica-type
    :reader form-type
    :initarg :form-type)
   (bound-vars
    :type list
    :reader bound-vars
    :initarg :bound-vars)))
(defclass silica-literal (term)
  ((val
    :type t
    :reader val
    :initarg :val)
   (val-type
    :type silica-type
    :reader val-type
    :initarg :val-type)))
;; Lambda Related
(defclass term-lambda (term)
  ((var
    :type symbol
    :reader var
    :initarg :var)
   (var-type
    :type (or silica-type kind)
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

;; Structure related
(defclass silica-struct (term)
  ((entries
    :type list
    :reader entries
    :initarg :entries)))
(defclass projection (term)
  ((silica-struct
    :type term
    :reader silica-struct
    :initarg :structure)
   (field
    :type symbol
    :reader field
    :initarg :field)))

;; Recursive datatype-related
(defclass inductive-ctor (term)
  ((val-type
    :type silica-type
    :reader val-type
    :initarg :val-type)
   (name
    :type symbol
    :reader name
    :initarg :name)))

(defclass match-clause ()
  ((body
    :type term
    :reader body
    :initarg :body)
   (pattern
    :reader pattern
    :initarg :pattern)))
(defclass pattern-match (term)
  ((term
    :type term
    :reader term
    :initarg :term)
   (clauses
    :type list
    :reader clauses
    :initarg :clauses)))

;; (defclass inductive-val (term)
;;   ((constructor
;;     :type inductive-ctor
;;     :reader constructor
;;     :initarg constructor)
;;    (vals
;;     :type (vector term)
;;     :reader vals
;;     :initarg :vals)))

;; yuck! can we remove him via laziness?
(defclass conditional (term)
  ((test
    :type term
    :reader test
    :initarg :test)
   (if-true
    :type term
    :reader if-true
    :initarg :if-true)
   (if-false
    :type term
    :reader if-false
    :initarg :if-false)))


(defclass silica-type () ())
(defclass type-var (silica-type)
  ((var
    :type symbol
    :reader var
    :initarg :var)))
(defclass native-type (silica-type)
  ((native-type
    :type t
    :reader native-type
    :initarg :native-type)))
(defclass arrow (silica-type)
  ((from
    :type silica-type
    :reader from
    :initarg :from)
   (to
    :type silica-type
    :reader to
    :initarg :to)))
(defclass tapp (silica-type)
  ((left
    :type silica-type
    :reader left
    :initarg :left)
   (right
    :type silica-type
    :reader right
    :initarg :right)))
(defclass forall (silica-type)
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
    :type silica-type
    :reader body
    :initarg :body)))
(defclass type-lambda (term)
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
(defclass signature (silica-type)
  ((entries
    :type list
    :reader entries
    :initarg :entries)))
(defclass inductive-type (silica-type)
  ((var
    :type symbol
    :reader var
    :initarg :var)
   (kind
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


;; Untyped declarations and definitions
;; these are replaced with either type or val declarations or definitions during typechecking.
(defclass silica-declaration ()
  ((var
    :type symbol
    :reader var
    :initarg :var)
   (ann
    :type t
    :reader ann
    :initarg :ann)))
(defclass silica-definition ()
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
  (make-instance 'silica-declaration :var var :ann ann))
(defun mk-def (var val)
  (make-instance 'silica-definition :var var :val val))
(defun mk-entry (var bind)
  (make-instance 'entry :var var :binder bind))

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
(defun mk-sig (entries)
  (make-instance 'signature :entries entries))

;; Term constructors
(defun mk-lisp (type form)
  (make-instance 'lisp-form :form-type type :form form))
(defun mk-λ (var snd &optional body)
  (if body
      (make-instance 'silica-lambda
                     :var var :var-type snd :body body)
      (make-instance 'silica-lambda :var var :body snd)))
(defun mk-tλ (var snd &optional body)
  (if body
      (make-instance 'type-lambda
                     :var var :var-kind snd :body body)
      (make-instance 'type-lambda :var var :body snd)))
(defun mk-mλ (var snd &optional body)
  (if body
      (make-instance 'term-lambda
                     :var var :var-type snd :body body)
      (make-instance 'term-lambda :var var :body snd)))
(defun mk-abs (var snd &optional body)
  (if body
      (make-instance 'abstract :var var :var-kind snd :body body)
      (make-instance 'abstract :var var :body snd)))
(defun mk-var (var)
  (make-instance 'var :var var))
(defun mk-tvar (var)
  (make-instance 'type-var :var var))
(defun mk-mvar (var)
  (make-instance 'term-var :var var))
(defun mk-app (left right)
  (make-instance 'app :left left :right right))
(defun mk-struct (defs)
  (make-instance 'silica-struct :entries defs)) 

(declaim (ftype (function (symbol (or kind list) &optional list) inductive-type) mk-induct))
(defun mk-induct (var k-or-c &optional constructors)
  (if (not constructors)
      (make-instance 'inductive-type
                     :var var
                     :constructors k-or-c)
      (make-instance 'inductive-type
                     :var var
                     :kind k-or-c
                     :constructors constructors)))

(declaim (ftype (function (term list) pattern-match) mk-match))
(defun mk-match (term clauses)
  (make-instance 'pattern-match :term term :clauses clauses))

(declaim (ftype (function (inductive-type symbol) inductive-ctor) mk-ctor))
(defun mk-ctor (type name)
  (make-instance 'inductive-ctor
                 :val-type type
                 :name name))

(defun mk-proj (field struct)
  (make-instance 'projection :structure struct :field field))
(defun mk-val (val)
  (make-instance 'silica-literal :val val))
(defun mk-if (test if-true if-false)
  (make-instance 'conditional :test test
                              :if-true if-true
                              :if-false if-false))

(defgeneric atomic? (term)
  (:method ((term var)) t)
  (:method ((term type-var)) t)
  (:method ((term term-var)) t)
  (:method ((term silica-literal)) t)
  (:method ((term kind-type)) t)
  (:method (term) nil))

(defgeneric get-field (term field)
  (:method ((term silica-struct) field)
    (iter (for elt in (entries term))
      (when (and (eq (var elt) field)
                 (typep (binder elt) 'silica-definition))
        (return (val (binder elt))))))

  (:method ((term signature) field)
    (iter (for entry in (entries term))
      (when (eq (var entry) field)
        (return (ann (binder entry))))))

  ;; TODO: fix me??
  (:method ((term hash-table) field)
    (gethash field term)))

(defgeneric α= (l r &optional renamings shadowed)
  (:documentation "Predicate: return true if two terms equal up to α-renaming")
  ;; Terms 
  (:method ((l silica-literal) (r silica-literal) &optional renamings shadowed)
    (declare (ignore renamings shadowed))
    (equal (val l) (val r)))

  (:method ((l silica-lambda) (r silica-lambda) &optional renamings shadowed)
    (and 
     (or (and (not (slot-boundp l 'var-type)) (not (slot-boundp r 'var-type)))
         (α= (var-type l) (var-type r)))
     (α= (body l) (body r)
         (acons (var l) (var r) renamings)
         (cons (cons (var l) (car shadowed))
               (cons (var l) (car shadowed))))))

  (:method ((l term-lambda) (r term-lambda) &optional renamings shadowed)
    (and 
     (or (and (not (slot-boundp l 'var-type)) (not (slot-boundp r 'var-type)))
         (α= (var-type l) (var-type r)))
     (α= (body l) (body r)
         (acons (var l) (var r) renamings)
         (cons (cons (var l) (car shadowed))
               (cons (var l) (car shadowed))))))

  (:method ((l type-lambda) (r type-lambda) &optional renamings shadowed)
    (and 
     (or (and (not (slot-boundp l 'var-kind)) (not (slot-boundp r 'var-kind)))
         (α= (var-kind l) (var-kind r)))
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

  (:method ((l silica-struct) (r silica-struct) &optional renamings shadowed)
    (and 
     (iter (for elt-1 in (entries l))
           (for elt-2 in (entries r))
       (always
        (and (eq (var elt-1) (var elt-2))
             ;; todo: shadow variable bindings?
             (typecase (cons (binder elt-1) (binder elt-2))
               ((cons silica-declaration silica-declaration)
                (α= (binder elt-1) (binder elt-2) renamings shadowed))
               ((cons silica-definition silica-definition)
                (α= (binder elt-1) (binder elt-2) renamings shadowed))))))
     (= (length (entries l)) (length (entries r)))))

  (:method ((l inductive-type) (r inductive-type) &optional renamings shadowed)
    (and 
     (α= (kind l) (kind r))
     (every
      (lambda (l-decl r-decl)
        (α= l-decl r-decl
            (acons (var l) (var r) renamings)
            (cons (cons (var l) (car shadowed))
                  (cons (var l) (car shadowed)))))
      (constructors l) (constructors r))))

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
    
  (:method ((l term-var) (r term-var) &optional renamings shadowed)
    (let ((entry (assoc (var l) renamings)))
      (if entry
          (eq (cdr entry) (var r))
          (and (eq (member (var l) (car shadowed))
                   (member (var r) (cdr shadowed)))
               (eq (var l) (var r))))))
    
  (:method ((l type-var) (r type-var) &optional renamings shadowed)
    (let ((entry (assoc (var l) renamings)))
      (if entry
          (eq (cdr entry) (var r))
          (and (eq (member (var l) (car shadowed))
                   (member (var r) (cdr shadowed)))
               (eq (var l) (var r))))))

  (:method ((l signature) (r signature) &optional renamings shadowed)
    (and 
     (iter (for entry-1 in (entries l))
           (for entry-2 in (entries r))
       (always
        (and (eq (var entry-1) (var entry-2))
             (α= (binder entry-1) (binder entry-2) renamings shadowed))))
     (= (length (entries l)) (length (entries r)))))

  ;; Equality of defs/decls
  (:method ((l silica-definition) (r silica-definition)
            &optional renamings shadowed)
    (α= (val l) (val r)
        (acons (var l) (var r) renamings)
        (cons (cons (var l) (car shadowed))
              (cons (var r) (cdr shadowed)))))

  (:method ((l silica-declaration) (r silica-declaration)
            &optional renamings shadowed)
    (α= (ann l) (ann r)
        (acons (var l) (var r) renamings)
        (cons (cons (var l) (car shadowed))
              (cons (var r) (cdr shadowed)))))


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

(defun α>= (l r &optional renamings shadowed)
  (α<= r l
       (li:map (lambda (c) (cons (cdr c) (car c))) renamings)
       (cons (cdr shadowed) (car shadowed))))

(defgeneric α<= (l r &optional renamings shadowed)
  (:documentation "Predicate: return true if the lhs is a subtype of the rhs up to α-renaming")
  
  (:method ((l type-var) (r type-var) &optional renamings shadowed)
    (let ((entry (assoc (var l) renamings)))
      (if entry
          (eq (cdr entry) (var r))
          (and (eq (member (var l) (car shadowed))
                   (member (var r) (cdr shadowed)))
               (eq (var l) (var r))))))

  (:method ((l var) (r var) &optional renamings shadowed)
    (let ((entry (assoc (var l) renamings)))
      (if entry
          (eq (cdr entry) (var r))
          (and (eq (member (var l) (car shadowed))
                   (member (var r) (cdr shadowed)))
               (eq (var l) (var r))))))

  (:method ((l type-lambda) (r type-lambda) &optional renamings shadowed)
    (and 
     (or (and (not (slot-boundp l 'var-kind)) (not (slot-boundp r 'var-kind)))
         (α= (var-kind l) (var-kind r)))
     (α<= (body l) (body r)
          (acons (var l) (var r) renamings)
          (cons (cons (var l) (car shadowed))
                (cons (var l) (car shadowed))))))

  ;; TODO
  (:method ((l tapp) (r tapp) &optional renamings shadowed)
    (and
     (α<= (left l) (left r) renamings shadowed)
     (α<= (right l) (right r) renamings shadowed)))

  ;; Types
  (:method ((l native-type) (r native-type) &optional renamings shadowed)
    (declare (ignore renamings shadowed))
    (subtypep (native-type l) (native-type r)))

  (:method ((l forall) (r forall) &optional renamings shadowed)
    (α<= (body l) (body r)
         (acons (var l) (var r) renamings)
         (cons (cons (var l) (car shadowed))
               (cons (var r) (cdr shadowed)))))

  (:method ((l arrow) (r arrow) &optional renamings shadowed)
    (and (α<= (from l) (from r) renamings shadowed)
         (α>= (to l) (to r) renamings shadowed)))

  (:method ((l signature) (r signature) &optional renamings shadowed)
    ;; TODO: lots of work here
    (and 
     (iter (for entry-1 in (entries l))
       (for entry-2 in (entries r))
       (always
        (and (eq (var entry-1) (var entry-2))
             (α= (binder entry-1) (binder entry-2) renamings shadowed))))
     (= (length (entries l)) (length (entries r)))))

  ;; Equality of defs/decls
  (:method ((l silica-definition) (r silica-definition)
            &optional renamings shadowed)
    (α<= (val l) (val r)
         (acons (var l) (var r) renamings)
         (cons (cons (var l) (car shadowed))
               (cons (var r) (cdr shadowed)))))

  (:method ((l silica-declaration) (r silica-declaration)
            &optional renamings shadowed)
    (α<= (ann l) (ann r)
         (acons (var l) (var r) renamings)
         (cons (cons (var l) (car shadowed))
               (cons (var r) (cdr shadowed)))))

  (:method ((l inductive-type) (r inductive-type) &optional renamings shadowed)
    (and 
     (α= (kind l) (kind r))
     (every
      (lambda (l-decl r-decl)
        (α<= l-decl r-decl
             (acons (var l) (var r) renamings)
             (cons (cons (var l) (car shadowed))
                   (cons (var r) (car shadowed)))))
      (constructors l) (constructors r))))

  (:method ((l kind) (r kind) &optional renamings shadowed)
    (α= l r renamings shadowed))

  ;; otherwise: false
  (:method ((l silica-type) (r silica-type) &optional renamings shadowed)
    (declare (ignore l r renamings shadowed))
    nil))

(declaim (ftype (function ((or term kind silica-type)
                           &optional (function (t) boolean))
                          string)
                mparen))
(defun mparen (val &optional (test-func #'atomic?))
  (if (funcall test-func val)
      (show val)
      (concatenate 'string "(" (show val) ")")))

;; TODO: document/layout engine (truncate at n chars)
(defgeneric show (val)
  (:documentation "Pseudo pretty-print method")

  (:method ((val silica-literal))
    (format nil "~A" (val val)))

  (:method ((val lisp-form))
    (format nil "(lisp ~A ~A ~A)" (form-type val) nil (form val)))

  (:method ((val var))
    (string (var val)))
  (:method ((val term-var))
    (concatenate 'string "tm/" (string (var val))))
  (:method ((val type-var))
    (concatenate 'string "ty/" (string (var val))))

  (:method ((val app))
    (let ((stream (make-string-output-stream)))
      (write-string (mparen (left val)) stream)
      (write-string " " stream)
      (write-string (mparen (right val)) stream)
      (get-output-stream-string stream)))

  (:method ((val pattern-match))
    (let ((stream (make-string-output-stream)))
      (format stream "φ ~A" (show (term val)))
      (iter (for clause in (clauses val))
        (format stream " (~A → ~A)" (pattern clause) (show (body clause))))
      (get-output-stream-string stream)))

  (:method ((val silica-lambda))
    (format nil "λ ~A. ~A" (var val) (show (body val))))
  (:method ((val type-lambda))
    (format nil "tλ ~A. ~A" (var val) (show (body val))))
  (:method ((val term-lambda))
    (format nil "mλ ~A. ~A" (var val) (show (body val))))

  (:method ((val abstract))
    (format nil "Λ ~A. ~A" (var val) (show (body val))))

  (:method ((val silica-struct))
    (let ((stream (make-string-output-stream)))
      (write-string "(σ" stream )
      (iter (for entry in (entries val))
        (typecase (binder entry)
          (silica-declaration
           (format stream " (~A(~A) ⮜ ~A)"
                   (var entry)
                   (var (binder entry))
                   (show (ann (binder entry)))))
          (silica-definition
           (format stream " (~A(~A) ≜ ~A)"
                   (var entry)
                   (var (binder entry))
                   (show (val (binder entry)))))))
      (write-string ")" stream)
      (get-output-stream-string stream)))

   (:method ((val conditional))
    (format nil "if ~A ~A ~A"
            (mparen (test val))
            (mparen (if-true val))
            (mparen (if-false val))))

  (:method ((val projection))
    (format nil "~A⋅~A"
            (mparen (silica-struct val))
            (field val)))


  ;; For types
  (:method ((type native-type))
    (format nil "~A" (native-type type)))

  (:method ((type arrow))
    (format nil "~A → ~A"
            (mparen (from type))
            (mparen (to type)
                    (lambda (v) (or (atomic? v) (typep v 'arrow))))))

  (:method ((type forall))
    (format nil "∀ ~A. ~A" (var type) (show (body type))))

  (:method ((type tapp))
    (let ((stream (make-string-output-stream)))
      (write-string (mparen (left type)) stream)
      (write-string " " stream)
      (write-string (mparen (right type)) stream)
      (get-output-stream-string stream)))

  (:method ((type inductive-type))
    (let ((stream (make-string-output-stream)))
      (write-string "Φ" stream)
      (when-slot (var type 'var) (format stream " ~A " var))
      (iter (for ctor in (constructors type))
        (format stream " (~A ⮜ ~A)"
                (var ctor)
                (show (ann ctor))))
      (get-output-stream-string stream)))

  (:method ((type inductive-ctor))
    (let ((stream (make-string-output-stream)))
      (format stream "(ctor : ~A)" (name type))
      ;;(write-string " ⮜ " stream)
      ;; (format stream "~A" (val-type type))
      (get-output-stream-string stream)))

  (:method ((type signature))
    (let ((stream (make-string-output-stream)))
      (write-string "Σ" stream )
      (iter (for entry in (entries type))
        (format stream " (~A(~A) ⮜ ~A)"
                (var entry)
                (var (binder entry))
                (show (ann (binder entry)))))
      (get-output-stream-string stream)))
  
  ;; For Kinds
  (:method ((kind kind-type)) "κ")

  (:method ((kind kind-arrow))
    (format nil "(~A → ~A)" (show (from kind)) (show (to kind))))

  (:method ((decl silica-declaration))
    (format nil "(~A ⮜ ~A)"  (var decl) (show (ann decl))))

  (:method ((defn silica-definition))
    (format nil "(~A ≜ ~A)"  (var defn) (show (val defn))))

  (:method (unknown)
    (format nil "Cannot Show object of type ~A" (type-of unknown))))


(defmethod print-object ((term term) stream)
  (format stream "#<term ~A>" (show term)))

(defmethod print-object ((type silica-type) stream)
  (format stream "#<type ~A>" (show type)))

(defmethod print-object ((kind kind) stream)
  (format stream "#<kind ~A>" (show kind)))

(defmethod print-object ((decl silica-declaration) stream)
  (format stream "#<decl ~A>" (show decl)))

(defmethod print-object ((def silica-definition) stream)
  (format stream "#<def ~A>" (show def)))

