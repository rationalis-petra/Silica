(in-package :opal)

;; Opal programming langugae
;; Compiles to common lisp
;; FFI via common lisp


(defvar *opal-symbols*
  (make-package "opal-symbols"))
(defun sym (name) (intern name *opal-symbols*))


(defclass module ()
  ((opl-imports)
   (opl-exports)
   (opal-struct)))


(defclass term () ())
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
   (type
    :type opl-type)
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
(defclass opl-int (term)
  ((val
    :type integer
    :reader val
    :initarg :val)))


(defclass opl-type () ())
(defclass signature (opl-type)
  ((declarations
    :type list)))
(defclass arrow (opl-type)
  ((from
    :type opl-type)
   (to
    :type opl-type)))
(defclass forall (opl-type)
  ((var
    :type symbol)
   (body
    :type opl-type)))

(defclass kind () ())
(defclass kind-univ (kind) ())
(defclass kind-arr (kind)
  ((from
    :type kind
    :reader from)
   (kind-to
    :type kind
    :reader to)))


(defclass type-declaration ()
  ((var
    :type symbol
    :reader var
    :initarg :var)
   (ann
    :type opl-type
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
    :type opl-type
    :reader val
    :initarg :val)))
;; ;; bidirectional typechecking

;; (defgeneric check (term type context))
;; (defgeneric infer (term context))

;; ;; evaluation via β-reduction
(defgeneric reify (term environment)
  (:method ((term opal-lambda) env)
    "Reify a lambda term (λ x.e). These are converted into CL lambdas"
    `(lambda (,(var term))
       ,(reify (body term) env)))

  (:method ((term quantify) env)
    "Reify a type forall term (Λ α.e). These are erased at runtime, and so are
equivalent to their bodies." 
    (reify (body term) env))

  (:method ((term var) env)
    "Reify a var (x). This is trivial, just extract the var's symbol"
    (var term))

  (:method ((term app) env)
    "Reify an application (e e'). An application may be either a type or term
application. If it is a type application, then we erase it. Otherwise, convert
it to a funcall."
    (typecase (right term)
      (opl-type (reify (left term) env))
      (term `(funcall ,(reify (left term) env) ,(reify (right term) env)))))


  (:method ((term opal-struct) env)
    "Reify a structure (struct (x₁ ≜ e₁) .. (xₙ ≜ eₙ)). We do this by
constructing a (let* ((x₁ e₁) .. (xₙ eₙ)) (hashmap (x₁ = e₁) .. (xₙ = eₙ)))"
    (labels
        ((mk-binds (defs)
           (iter (for def in defs)
             (collect (list (var def) (reify (val def) env)))))
         (mk-hashmap (vars)
           ;; We can safely use a non-hygenic macro, as all opal code will only
           ;; contain symbols in the *opal-symbols* package.
           `(let ((hashmap (make-hash-table :test #'eq)))
              ,@(iter (for var in vars)
                  (collect `(setf (gethash (quote ,var) hashmap) ,var)))
              hashmap)))
      `(let
           (,@(mk-binds (entries term)))
         ,(mk-hashmap (mapcar #'var (entries term))))))

  (:method ((term projection) env)
    "Reify a field access (s.f). Do do this by converting it to (gethash 'f s)"
    `(gethash (quote ,(field term)) ,(reify (opal-struct term) env)))

  (:method ((term symbol) env) term)

  (:method ((term opl-int) env) (val term))
  (:method (term env) term))

;; TEST AREA
(defun mk-λ (var body)
  (make-instance 'opal-lambda :var var :body body))
(defun mk-var (var)
  (make-instance 'var :var var))
(defun mk-app (left right)
  (make-instance 'app :left left :right right))
(defun mk-struct (defs)
  (make-instance 'opal-struct :entries defs)) 
(defun mk-proj (struct field)
  (make-instance 'projection :structure struct :field field))
(defun mk-int (val)
  (make-instance 'opl-int :val val))
(defun mk-def (var val)
  (make-instance 'val-definition :var var :val val))

(defun to-def (definition)
  (let ((name (car definition))
        (args (iter (for elt in (cdr definition))
                (if (eq elt '≜)
                    (finish)
                    (collect elt))))
        (body (iter (for elt in (cdr definition))
                    (with start = nil)
                (when start (collect elt))
                (when (eq elt '≜) (setf start t)))))
    (flet ((mk-fun (args expr)
             (reduce (lambda (x y) (mk-λ y x))
                     (cons expr (reverse args))))

           (body-expr (body)
             (if (= 1 (length body))
                 (to-ast (car body))
                 (reduce #'mk-app (mapcar #'to-ast body)))))
      (mk-def name
              (if args
                  (mk-fun args (body-expr body))
                  (body-expr body))))))


(defun to-ast (term)
  (match term
    ((type symbol) (mk-var term))
    ((type integer) (mk-int term))
    ((type list)
     (match (car term)
       ('λ
        (match (cadr term)
          ((type symbol) (mk-λ (cadr term) (to-ast (caddr term))))
          ((type list) (reduce (lambda (x y) (mk-λ y x))
                               (cons (to-ast (caddr term))
                                     (reverse (cadr term)))))))
       ('σ
        (mk-struct (mapcar #'to-def (cdr term))))
       ('π
        (mk-proj (to-ast (caddr term)) (cadr term)))
       (_ (reduce #'mk-app (mapcar #'to-ast term)))))
    (_ (format t "failed to match:~A~%" term))))


(defmacro opal-toplevel (term)
  (reify (to-ast term) nil))

