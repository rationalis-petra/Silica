(in-package :opal)

;;(defstruct env )

;; Bind var to ty in env.
(declaim (ftype (function (symbol (or opal-type kind) list) cons) bind))
(defun bind (var ty env)
  (cons (acons var ty (car env)) (acons var nil (cdr env))))


(defparameter +empty-env+ (cons nil nil))

;; Bind var to val in env.
;; Mostly for, e.g. looking up types in the environment. 
(defun bind-existing-val (var val env)
  (labels ((build-vals (var val alist)
             (match alist
               ((guard (cons (cons nvar _) rest)
                       (eq var nvar))
                (acons var val rest))
               ((cons (cons nvar nval) rest)
                (acons nvar nval (build-vals var val rest)))
               (nil (error "Tried to insert val for non-bound var ~A" var)))))
    (cons (car env)
          ;; iterate down the value env until we find a key
          (build-vals var val (cdr env)))))

(defun bind-2 (var ty val env)
  (cons (acons var ty (car env)) (acons var val (cdr env))))

(declaim (ftype (function (symbol list) (or null opal-type kind)) lookup))
(defun lookup (var env)
  (or (cdr (assoc var (car env)))
      (error (format nil "Can't find variable ~A in environment" var))))

(defun lookup-val (var env)
  (or (cdr (assoc var (car env)))
      (error (format nil "Can't find value of type ~A in environment" var))))

(defun join (env-1 env-2)
  (cons (append (car env-1) (car env-2))
        (append (cdr env-1) (cdr env-2))))


;; (defgeneric validate-type (ty env)
;;   (:method ((type var))
;;     (when (typep (lookup ty env) 'kind)
;;       (mk-tvar (var type))))
;;   (:method ((type forall))
;;     ()
;;     )
;;   (:method ((type signature)))
;;   (:method ((type arrow))
;;     (mk-arr (validate-type (left type) env)
;;             (validate-type (right type) env)))
;;   (:method ((type native)) type))

(defgeneric ty-subst (type subt)
  (:method ((type var) subst)
    (or (cdr (assoc (var type) subst))
        (mk-tvar (var type))))
  (:method ((type type-var) subst)
    (or (cdr (assoc (var type) subst))
        type))

  (:method ((type tapp) subst)
    (mk-tapp (ty-subst (left type) subst) (ty-subst (right type) subst)))

  (:method ((type app) subst)
    (mk-tapp (ty-subst (left type) subst) (ty-subst (right type) subst)))

  (:method ((type forall) subst) 
    (if (slot-boundp type 'var-kind)
        (mk-∀
         (var type)
         (var-kind type)
         (ty-subst (body type)
                   (remove-if (lambda (x) (eq (car x) (var type))) subst)))
        (mk-∀
         (var type)
         (ty-subst (body type)
                   (remove-if (lambda (x) (eq (car x) (var type))) subst)))))

  (:method ((type arrow) subst) 
    (mk-arr
     (ty-subst (from type) subst)
     (ty-subst (to type) subst)))

  (:method ((type native-type) subst) type)
  (:method ((kind kind) subst) kind))


(defgeneric ty-reduce (type)
  (:method ((type forall))
    (mk-∀ (var type) (var-kind type)
          (ty-reduce (body type))))

  (:method ((type arrow))
    (mk-arr (ty-reduce (from type))
            (ty-reduce (to type))))

  (:method ((type app)) (ty-reduce (mk-tapp (left type) (right type))))
  (:method ((type tapp))
    (let ((left-v2 (ty-reduce (left type))))
      (typecase left-v2
        (forall (ty-subst (body left-v2)
                          (acons (var left-v2)
                                 (ty-reduce (right type))
                                 nil)))
        (t (mk-tapp left-v2 (ty-reduce (right type)))))))

  (:method ((type signature))
    (mk-sig
     (mapcar (lambda (decl)
               (mk-decl (var decl) (ty-reduce (ann decl))))
             (declarations type))))

  (:method ((type type-var)) type)
  (:method ((type var)) type)
  (:method ((type native-type)) type)
  (:method ((kind kind)) kind))

(defun ty-eval (type env)
  (let* ((vals (iter (for entry in (cdr env))
                (when (cdr entry) (collect entry))))
         (val (ty-reduce (ty-subst type vals))))
    val))

;; α-reduce= : 
(defun α-r= (l r env)
  "Tests for α-equality in environment env. This allows substituting types for
values, to deal with existential typing."
    (α= (ty-eval l env) (ty-eval r env)))



;; we use biridrectional typechecking
;; i.e. 1x check method, 1x infer method
(declaim (ftype (function (t t t) (or term opal-type)) check))
(defgeneric check (term type env)
  (:documentation "Perform typechecking.")

  (:method ((term lisp-form) type env)
    (if (α-r= type (form-type term) env)
        term
        (error
         (format nil "Types not equal: ~A and ~A~%" type (form-type term)))))

  (:method ((term var) type env)
    (if (α-r= type (lookup (var term) env) env)
        (typecase type
          (kind (mk-tvar (var term)))
          (opal-type (mk-mvar (var term))))
        (error (format nil "Var ~A does not have type ~A, but rather~A~%"
                       (var term) type (lookup (var term) env)))))

  (:method ((term term-var) type env)
    (if (α-r= type (lookup (var term) env) env)
        term
        (error (format nil "Var ~A does not have type ~A, but rather~A~%"
                       (var term) type (lookup (var term) env)))))

  (:method ((term type-var) type env)
    (if (α-r= type (lookup (var term) env) env)
        term
        (error (format nil "Var ~A does not have type ~A, but rather~A~%"
                       (var term) type (lookup (var term) env)))))

  (:method ((term opal-literal) (type native-type) env)
    (if (typep (val term) (native-type type))
        term
        (error (format nil "Term ~A does not have type ~A~%" term type))))

  ;; TODO: also check if type is a kind-arrow, for system Fω 
  (:method ((term opal-lambda) (type arrow) env)
    (flet ((body-check (from to)
             (check (body term) to (bind (var term) from env))))
      (if (slot-boundp term 'var-type)
          (if (α-r= (from type) (check (var-type term) (mk-kind) env) env)
              (body-check (var-type term) (to type))
              (error "Declared function argument type doesn't match actual type"))
          (mk-λ (var term)
                (from type)
                (body-check (from type) (to type))))))

  (:method ((term abstract) (type forall) env)
    (flet ((body-check (from to)
             ;; TODO: perform α-conversion/equality, as ∀ and Λ may bind
             ;; different variables!
             ))
      ;; TODO: 
      (unless (equal (var term) (var type))
        (error (format nil "Limitation in typechecker: ∀ and Λ must have same
    symbol, have ~A and ~A" (var term) (var type))))
      (unless (or (not (slot-boundp term 'var-kind))
                  (not (slot-boundp type 'var-kind))
                  (α= (var-kind term) (var-kind type)))
        (error (format nil "Type check failed due to kind mismatch")))
      
      (mk-abs (var term) (var-kind type)
              (check (body term) (body type)
                     (bind (var term) (var-kind type) env)))))

  (:method ((term opal-struct) (type signature) env)
    (labels ((has-repeating-field (list)
               (cond
                 ((null list) nil)
                 ((member (car list) (cdr list)) t)
                 (t (has-repeating-field (cdr list))))))

      ;; If it has repeating fields, typecheck fails
      (when (has-repeating-field
             (iter (for entry in (entries term))
               (when (typep entry 'opal-definition)
                 (collect (var entry)))))
        (error "Repeating definitions in term: ~A" term))

      (mk-struct
       (iter (for entry in (entries term))
         (with decls = (declarations type))
         ;; locals = local declarations
         (with locals = nil)
         (with prev-decl = nil)
         ;; (with type-vals = nil)
         (typecase entry
           (opal-definition
            (let* ((decl (or prev-decl (pop decls)))
                   (new-val
                     (check
                      (val entry)
                      (ty-eval (ann decl) (join locals env))
                      (join locals env))))

              (unless (eq (var entry) (var decl))
                (error "Definition variable not equal to declaration variable: ~A ~A"
                       (var entry) (var decl)))


              ;; Update locals
              (typecase new-val
                (term
                 ;; if current definition defines a term, check if it's been declared
                 ;; if not, add it into the locals
                 (unless prev-decl
                   (setf locals (bind (var decl) (ann decl) locals))))
                (opal-type
                 ;; if current definition defines a type, add it's kind and
                 ;; value into local
                 (if prev-decl
                     (setf locals (bind-existing-val (var decl) new-val locals))
                     (setf locals (bind-2 (var decl) (ann decl) new-val locals)))))
              (setf prev-decl nil)

              (collect decl)
              (collect (mk-def (var entry) new-val))))
           (opal-declaration
            (let ((decl (pop decls)))
              (when prev-decl
                (error "Can't have two declarations in a row"))
              (unless (α-r= entry decl env) ;; TODO: substitute value!
                (error "Can't deal with declarations in structures (σ)"))
              (setf prev-decl decl)
              (setf locals (bind (var decl) (ann decl) locals)))))))))

  (:method ((term projection) (type opal-type) env)
    (let ((struct-ty (infer (opal-struct term) env)))
      (α-r= (get-field struct-ty (field term)) type env)))

  (:method ((term app) (type opal-type) env)
    (let* ((left-result (infer (left term) env))
           (right-result (infer (right term) env))

           (lt (car left-result))
           (lv (cdr left-result))
           (rt (car right-result))
           (rv (cdr right-result)))
      (typecase lt
        (arrow
         (unless (and (α-r= (from lt) rt env) (α-r= (to lt) type env))
           (error "Bad application of function")))
        (forall
         (unless (and (or (not (slot-boundp lt 'var-kind))
                      (α= (var-kind lt) rt))
                  (α-r= (ty-reduce (mk-tapp lt (right term)))
                        type
                        env))
           (error "Bad application of abstraction")))
        (t (error "Applying to neither function or abstraction")))
      (mk-app lv rv)))

  ;; Kind Checking
  (:method ((type native-type) (kind kind-type) env) type)
  (:method ((type arrow) (kind kind-type) env)
    ;; TODO: iterate through and check for well-formedness
    type)
  (:method ((type signature) (kind kind-type) env)
    ;; TODO: iterate through & check for well-formedness
    type)

  (:method ((type forall) (kind kind-arrow) env)
    (if 
     (α= (from kind)
           (if (slot-boundp type 'var-kind) (var-kind type) (mk-kind)))
     (mk-∀ (var type) (from kind)
           (check (body type) (to kind) (bind (var type) (from kind) env)))
     (error "Kind check failed, ~A ≠ ~A"
            (from kind)
            (if (slot-boundp type 'var-kind) (var-kind type) (mk-kind)))))

  ;; Failure cases
  (:method ((term term) (type opal-type) env)
    (error (format nil "Failed to typecheck term ~A as type ~A~%" term type)))

  (:method ((type opal-type) (kind kind) env)
    (error (format nil "Failed to typecheck type ~A as kind ~A~%" type kind)))

  (:method ((term term) (kind kind) env)
    (error (format nil "Failed to typecheck term ~A as kind ~A~%. Note: Terms to
    not have kinds!" term kind))))


(declaim (ftype (function (t t) (or (cons term opal-type) (cons opal-type kind))) infer))
(defgeneric infer (term env)
  (:method ((term opal-literal) env)
    (cons 
     (mk-native
      ;; TODO: formalize somewhere the default inference/correspondence
      ;; lisp type ↔ opal type
      (typecase (val term)
        (integer 'integer)
        (t (type-of (val term)))))
     term))

  (:method ((term var) env)
    (let ((ty (lookup (var term) env)))
      (if ty
          (cons ty
                (typecase ty
                  (kind (mk-tvar (var term)))
                  (opal-type (mk-mvar (var term)))))
          (error (format nil "No type found in environment for variable ~A"
                         (var term))))))
  
  (:method ((term type-var) env)
    (let ((ty (lookup (var term) env)))
      (if ty
          (cons ty term)
          (error (format nil "No type found in environment for variable ~A"
                         (var term))))))

  (:method ((term term-var) env)
    (let ((ty (lookup (var term) env)))
      (if ty
          (cons ty term)
          (error (format nil "No type found in environment for variable ~A"
                         (var term))))))

  (:method ((term opal-lambda) env)
    (if (slot-boundp term 'var-type)
        (let* ((arg-ty (check (var-type term) (mk-kind) env))
               (body-result (infer (body term)
                                   (bind (var term) arg-ty env))))
          (cons 
           (mk-arr arg-ty (car body-result))
           (mk-λ (var term) arg-ty (cdr body-result))))
        (error "To infer type of lambda, arguments must be annotated.")))

  (:method ((term abstract) env)
    ;; TODO: kind polymorphism
    (let* ((kind (if (slot-boundp term 'var-kind) (var-kind term) (mk-kind)))
           (body-result (infer (body term)
                               (bind (var term) kind env))))
      (cons 
       (mk-∀ (var term) (car body-result))
       (mk-abs (var term) kind (cdr body-result)))))

  (:method ((term app) env)
    (let* ((left-result (infer (left term) env))
           (right-result (infer (right term) env))

           (lt (car left-result))
           (lv (cdr left-result))
           (rt (car right-result))
           (rv (cdr right-result)))
      (typecase lt
        (arrow
         (if (α-r= (from lt) rt env)
             (cons (to lt) (mk-app lv rv))
             (error "Bad application of type ~A to ~A" lt rt)))
        ;; TODO instead of checking if it IS a forall, check that the type has kind _ → _
        (forall
         (if (α= (var-kind lt) rt)
             (cons 
              (ty-reduce (mk-tapp lt (right term)))
              (mk-app lv rv))
             (error "Bad application of abstraction: mismatched kinds ~A and ~A"
                    (var-kind lt) rt)))
        (t (error "Applying to neither function or abstraction of type: ~A" (show lt))))))

  (:method ((term opal-struct) env)
      (labels ((has-repeating-field (list)
               (cond
                 ((null list) nil)
                 ((member (car list) (cdr list)) t)
                 (t (has-repeating-field (cdr list))))))

        ;; If it has repeating fields, typecheck fails
        (when (has-repeating-field
               (iter (for entry in (entries term))
                 (when (typep entry 'opal-definition)
                   (collect (var entry)))))
          (error "Repeating definitions in term: ~A" term))

        (iter (for entry in (entries term))
          ;; locals = local declarations
          (with locals = nil)
          (with prev-decl = nil)
          ;; (with type-vals = nil)
          (typecase entry
            (opal-definition
             (let* ((new-entry
                      (if prev-decl 
                          (let ((ty (ty-eval (ann prev-decl) (join locals env))))
                            (cons ty (check (val entry) ty (join locals env))))
                          (infer
                           (val entry)
                           (join locals env))))
                    (new-ty (car new-entry))
                    (new-val (cdr new-entry)))

               (unless (or (not prev-decl) (eq (var entry) (var prev-decl)))
                 (error "Definition variable not equal to declaration variable: ~A ~A"
                        (var entry) (var prev-decl)))


               ;; Update locals
               (typecase new-ty
                 ;; if current definition defines a term, check if it's been declared
                 ;; if not, add it into the locals
                 (opal-type
                  (unless prev-decl
                    (setf locals (bind (var entry) new-ty locals))))
                 (kind
                  ;; if current definition defines a type, add it's kind and
                  ;; value into local
                  (if prev-decl
                      (setf locals (bind-existing-val (var entry) new-val locals))
                      (setf locals (bind-2 (var entry) new-ty new-val locals)))))

               (collect (or prev-decl (mk-decl (var entry) new-ty)) into out-decls)
               (collect (or prev-decl (mk-decl (var entry) new-ty)) into out-entries)
               (collect (mk-def (var entry) new-val) into out-entries)
               (setf prev-decl nil)))
            (opal-declaration
             (setf prev-decl entry)
             (setf locals (bind (var entry) (ty-eval (ann entry) (join locals env)) locals))))
          (finally (return (cons (mk-sig out-decls) (mk-struct out-entries)))))))

  (:method ((term projection) env)
    (let ((struct-result (infer (opal-struct term) env)))
      (cons 
       (get-field (car struct-result) (field term))
       (mk-proj (field term) (opal-struct term)))))

  (:method ((term term) env)
    (error (format nil "Cannot infer type of term ~A~%" term)))


  ;; Kind inference
  (:method ((type native-type) env)
    (cons 
     (mk-kind)
     type))

  (:method ((type forall) env)
    (let* ((body-result 
            (if (slot-boundp type 'var-kind)
                (infer (body type) (bind (var type) (var-kind type) env))
                (infer (body type) (bind (var type) (mk-kind) env))))
           (body-kind (car body-result))
           (body-type (cdr body-result)))
      (cons
       (mk-karr (if (slot-boundp type 'var-kind)
                    (var-kind type)
                    (mk-kind))
                body-kind)
       (mk-∀ (var type)
             (if (slot-boundp type 'var-kind)
                 (var-kind type)
                 (mk-kind))
             body-type)))))
