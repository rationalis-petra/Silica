(in-package :sigil)

(defvar *supress-print* nil)

;; we use bidirectional typechecking
;; i.e. 1x check method, 1x infer method
(declaim (ftype (function (t t t) (or term sigil-type)) check))
(defgeneric check (term type env)
  (:documentation "Perform typechecking.")

  (:method ((term lisp-form) type env)
    (if (β<= (form-type term) type env)
        term
        (error
         (format nil "Types not equal: ~A and ~A~%" type (form-type term)))))

  (:method ((term var) type env)
    (if (β<= type (lookup (var term) env) env)
        (typecase type
          (kind (mk-tvar (var term)))
          (sigil-type (mk-mvar (var term))))
        (error (format nil "Var ~A does not have type ~A, but rather~A~%"
                       (var term) type (lookup (var term) env)))))

  (:method ((term term-var) type env)
    (if (β<= type (lookup (var term) env) env)
        term
        (error (format nil "Var ~A does not have type ~A, but rather~A~%"
                       (var term) type (lookup (var term) env)))))

  (:method ((term type-var) type env)
    (if (β<= type (lookup (var term) env) env)
        term
        (error (format nil "Var ~A does not have type ~A, but rather~A~%"
                       (var term) type (lookup (var term) env)))))

  (:method ((term sigil-literal) (type native-type) env)
    (if (typep (val term) (native-type type))
        term
        (error (format nil "Term ~A does not have type ~A~%" term type))))

  (:method ((term sigil-lambda) (type arrow) env)
    (flet ((body-check (from to)
             (check (body term) to (bind (var term) from env))))
      (if (slot-boundp term 'var-type)
          (if (α-r= (from type) (check (var-type term) (mk-kind) env) env)
              (mk-mλ (var term) (from type) (body-check (var-type term) (to type)))
              (error "Declared function argument type doesn't match actual type"))
          (mk-mλ (var term)
                (from type)
                (body-check (from type) (to type))))))

  (:method ((term sigil-lambda) (type kind-arrow) env)
    (flet ((body-check (from to)
             (check (body term) to (bind (var term) from env))))
      (if (slot-boundp term 'var-type)
          (if (α= (from type) (var-type term))
              (body-check (var-type term) (to type))
              (error "Declared type constructor argument kind doesn't match actual kind"))
          (mk-tλ (var term)
                 (from type)
                 (body-check (from type) (to type))))))

  (:method ((term abstract) (type forall) env)
    (flet ((body-check (body type var1 var2 kind)
             (if (eq var1 var2)
                 (check body type (bind var2 kind env))
                 (check body
                        (ty-subst type (acons var1 (mk-tvar var2) nil))
                        (bind var2 kind env)))))
      ;; TODO: 
      (unless (or (not (slot-boundp term 'var-kind))
                  ;; (not (slot-boundp type 'var-kind))
                  (α= (var-kind term) (var-kind type)))
        (error (format nil "Type check failed due to kind mismatch")))
      
      (mk-abs (var term) (var-kind type)
              (body-check (body term) (body type) (var type) (var term) (var-kind type)))))

  (:method ((term sigil-struct) (type signature) env)
    (labels ((has-repeating-field (list)
               (cond
                 ((null list) nil)
                 ((member (car list) (cdr list)) t)
                 (t (has-repeating-field (cdr list))))))

      ;; If it has repeating fields, typecheck fails
      (when (has-repeating-field
             (iter (for entry in (entries term))
               (when (typep (binder entry) 'sigil-definition)
                 (collect (var entry)))))
        (error "Repeating definitions in term: ~A" term))

      (mk-struct
       (iter (for entry in (entries term))
             (for binder = (binder entry))
             (with decls = (entries type))
             (with locals = +empty-env+)
             (with prev-decl = nil)

         ;; locals = local declarations
         ;; (with type-vals = nil)
         (typecase binder
           (sigil-definition
            (let* ((decl-entry (or prev-decl (pop decls)))
                   (new-val
                     (check
                      (val binder)
                      (ty-eval (ann (binder decl-entry)) (join locals env))
                      (join locals env))))

              (unless (eq (var entry) (var decl-entry))
                (error "Definition variable not equal to declaration variable: ~A ~A"
                       (var entry) (var decl-entry)))


              ;; Update locals
              (typecase new-val
                (term
                 ;; if current definition defines a term, check if it's been declared
                 ;; if not, add it into the locals
                 (unless prev-decl
                   (setf locals (bind (var (binder decl-entry)) (ann (binder decl-entry)) locals))))
                (sigil-type
                 ;; if current definition defines a type, add it's kind and
                 ;; value into local
                 (if prev-decl
                     (setf locals (bind-existing-val (var (binder decl-entry)) new-val locals))
                     (setf locals (bind-2 (var (binder decl-entry)) (ann (binder decl-entry)) new-val locals)))))
              (setf prev-decl nil)

              (collect decl-entry)
              (collect (mk-entry (var entry) (mk-def (var entry) new-val)))))
           (sigil-declaration
            (let ((decl (pop decls)))
              (when prev-decl
                (error "Can't have two declarations in a row"))
              (unless (α-r= entry decl env) ;; TODO: substitute value!
                (error "Can't deal with declarations in structures (σ)"))
              (setf prev-decl decl)
              (setf locals (bind (var (binder decl)) (ann (binder decl)) locals)))))))))

  (:method ((term conditional) (type sigil-type) env)
    (let* ((test (check (test term) (mk-native 'boolean) env))
           (if-true (check (if-true term) type env))
           (if-false (check (if-false term) type env)))
      (mk-if test if-true if-false)))

  (:method ((term projection) (type sigil-type) env)
    (let* ((struct-result (infer (sigil-struct term) env))
           (struct-ty (car struct-result))
           (struct-val (cdr struct-result)))
      (if (α-r= (get-field struct-ty (field term)) type env)
          (mk-proj (field term) struct-val)
          (error "bad projection"))))

  (:method ((term app) (type sigil-type) env)
    (let* ((left-result (infer (left term) env))

           (lt (car left-result))
           (lv (cdr left-result)))
      (typecase lt
        (arrow
         (if (β<= (to lt) type env)
             (mk-app lv (check (right term) (ty-eval (from lt) env) env))
             (error (format nil "Bad application of function: ~A to ~A. ~%Expected output type ~A but got ~A"
                            (left term)
                            (right term)
                            type (to lt)))))
        (kind-arrow
         (if (α= (to lt) type)
             (mk-app lv (check (right term) (ty-eval (from lt) env) env))
             (error "Bad application of type constructor")))

        (forall
         ;; Assume kind is τ if not provided (TODO: kind inference!)
         (let* ((kind (if (slot-boundp lt 'var-kind)
                         (var-kind lt)
                         (mk-kind)))

                ;; Check that rhs has the expected kind 
                (right-val (check (right term) kind env)))

           ;; check that applying r to l gives the type we expect
           (unless (β<= (ty-subst (body lt) (acons (var lt) right-val nil))
                        type
                        env)
             (error "Bad application of abstraction"))

           (mk-tapp lv right-val)))
        (t (error "Applying to neither function or abstraction")))))

  ;; Kind Checking
  (:method ((type native-type) (kind kind-type) env) type)

  (:method ((type arrow) (kind kind-type) env)
    (mk-arr (check (from type) kind env) (check (to type) kind env)))

  (:method ((type signature) (kind kind-type) env)
    ;; TODO: iterate through & check for well-formedness
    type)

  (:method ((type forall) (kind kind-type) env)
    (let ((kind (if (slot-boundp type 'var-kind) (var-kind type) (mk-kind)))) 
     (mk-∀ (var type) kind
           (check (body type) (mk-kind) (bind (var type) kind env)))))

  ;; Failure cases
  (:method ((term term) (type sigil-type) env)
    (error (format nil "Failed to typecheck term ~A as type ~A~%" term type)))

  (:method ((type sigil-type) (kind kind) env)
    (error (format nil "Failed to typecheck type ~A as kind ~A~%" type kind)))

  (:method ((type-1 sigil-type) (type-2 sigil-type) env)
    (error (format nil "Failed to typecheck type ~A as type ~A~%" type-1 type-2)))

  (:method ((term term) (kind kind) env)
    (error (format nil "Failed to typecheck term ~A as kind ~A~%. Note: Terms to
    not have kinds!" term kind))))


(declaim (ftype (function (t t) (or (cons term sigil-type) (cons sigil-type kind))) infer))
(defgeneric infer (term env)

  (:method ((term lisp-form) env)
    (cons (ty-eval (form-type term) env)
        term))

  (:method ((term sigil-literal) env)
    (cons 
     (mk-native
      ;; TODO: formalize somewhere the default inference/correspondence
      ;; lisp type ↔ sigil type
      (type-of (val term)))
     term))

  (:method ((term var) env)
    (let ((ty (lookup (var term) env)))
      (if ty
          (cons ty
                (typecase ty
                  (kind (mk-tvar (var term)))
                  (sigil-type (mk-mvar (var term)))))
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

  (:method ((term sigil-lambda) env)
    (if (slot-boundp term 'var-type)
        (let* ((arg-ty (if (typep (var-type term) 'kind)
                           (ty-eval (var-type term) env)
                           (check (ty-eval (var-type term) env) (mk-kind) env)))
               (body-result (infer (body term)
                                   (bind (var term) arg-ty env))))
          (if
           (typep arg-ty 'kind)
           (cons
            (mk-karr arg-ty (car body-result))
            (mk-tλ (var term) arg-ty (cdr body-result)))
           (cons 
            (mk-arr arg-ty (car body-result))
            (mk-mλ (var term) arg-ty (cdr body-result)))))
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
         (if (β>= (from lt) rt env)
             (cons (to lt) (mk-app lv rv))
             (error "Bad application: term of type ~A cannot be applied to term of type ~A" rt lt)))
        (kind-arrow
         (if (α= (from lt) rt)
             (cons (to lt) (mk-tapp lv rv))
             (error "Bad application: type of kind ~A cannot be applied to type
           of kind ~A" rt lt)))
        (forall
         (if (α= (var-kind lt) rt)
             (cons 
              (ty-subst (body lt) (acons (var lt) (right term) nil))
              (mk-app lv rv))
             (error "Bad application of abstraction: mismatched kinds ~A and ~A"
                    (var-kind lt) rt)))
        (t (error "Applying to neither function or abstraction of type: ~A" (show lt))))))

  (:method ((term sigil-struct) env)
    (labels ((has-repeating-field (list)
               (cond
                 ((null list) nil)
                 ((member (car list) (cdr list)) t)
                 (t (has-repeating-field (cdr list))))))

      ;; If it has repeating fields, typecheck fails
      (when (has-repeating-field
             (iter (for entry in (entries term))
               (when (typep (binder entry) 'sigil-definition)
                 (collect (var entry)))))
        (error "Repeating definitions in term: ~A" term))

      (iter (for entry in (entries term))
        (for binder = (binder entry))
        ;; locals = local declarations
        (with locals = +empty-env+)
        (with prev-decl = nil)
        ;; (with type-vals = nil)
        (typecase binder
          (sigil-definition
           (let* ((new-entry
                    (if prev-decl 
                        (let ((ty (ty-eval (ann (binder prev-decl)) (join locals env))))
                          (cons ty (check (val binder) ty (join locals env))))
                        (infer
                         (val (binder entry))
                         (join locals env))))
                  (new-ty (car new-entry))
                  (new-val (cdr new-entry)))

             ;; TODO: account for renaming?
             (unless (or (not prev-decl) (eq (var entry) (var prev-decl)))
               (error "Definition variable not equal to declaration variable: ~A ~A"
                      (var entry) (var prev-decl)))


             ;; Update locals
             (typecase new-ty
               ;; if current definition defines a term, check if it's been declared
               ;; if not, add it into the local bindings
               (sigil-type
                (unless prev-decl
                  (setf locals (bind (var binder) new-ty locals))))
               (kind
                ;; if current definition defines a type, add it's kind and
                ;; value into local bindings
                (if prev-decl
                    (setf locals (bind-existing-val (var binder) new-val locals))
                    (setf locals (bind-2 (var binder) new-ty new-val locals)))))

             (collect (or (when prev-decl (binder prev-decl))
                          (mk-decl (var binder) new-ty))
               into out-decls)

             (collect (or (when prev-decl (binder prev-decl))
                          (mk-decl (var binder) new-ty))
               into out-entries)
             (collect (mk-def (var binder) new-val) into out-entries)
             (setf prev-decl nil)))
          (sigil-declaration
           (setf prev-decl entry)
           (setf locals (bind (var binder) (ty-eval (ann binder) (join locals env)) locals))))
        (finally (return
                   (cons
                    (mk-sig (li:map
                                ;; TODO: get the variable from elsewhere.
                             (lambda (x) (mk-entry (var x) x))
                             out-decls))
                    (mk-struct (li:map
                                ;; TODO: get the variable from elsewhere -
                                ;; perhaps the signature?
                                (lambda (x) (mk-entry (var x) x))
                                out-entries))))))))
  (:method ((term conditional) env)
    (let* ((test (check (test term) (mk-native 'boolean) env))
           (if-true (infer (if-true term) env))
           (if-false (infer (if-false term) env)))
      (α-r= (car if-true) (car if-false) env)
      (cons 
       (car if-true)
       (mk-if test if-true if-false))))

  (:method ((term projection) env)
    (let* ((struct-result (infer (sigil-struct term) env))
           (struct-ty (car struct-result))
           (struct-val (cdr struct-result)))
      (cons 
       (get-field struct-ty (field term))
       (mk-proj (field term) struct-val))))

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
