(in-package :opal)

;; we use biridrectional typechecking
;; i.e. 1x check method, 1x infer method
(defgeneric check (term type env)
  (:method ((term lisp-form) type env)
    (if (α= type (form-type term))
        term
        (error
         (format nil "Types not equal: ~A and ~A~%"
                 type
                 (form-type term)))))

  (:method ((term var) type env)
    (if (α= type (cdr (assoc (var term) env)))
        term
        (error (format nil "Var ~A does not have type ~A, but rather~A~%"
                       (var term) type (cdr (assoc (var term) env))))))

  (:method ((term opal-literal) (type native-type) env)
    (if (typep (val term) (lisp-type type))
        term
        (error (format nil "Term ~A does not have type ~A~%" term type))))

  ;; TODO: also check if type is a kind-arrow, for system Fω 
  (:method ((term opal-lambda) (type arrow) env)
    (flet ((body-check (from to)
             (check (body term) to (acons (var term) from env))))
      (if (slot-boundp term 'var-type)
          (if (α= (from type) (var-type term))
              (body-check (var-type term) (to type))
              (error "Declared function argument type doesn't match actual
    type"))
          (body-check (from type) (to type)))))

  (:method ((term quantify) (type forall) env)
    (flet ((body-check (from to)
             ;; TODO: perform α-conversion/equality, as ∀ and Λ may bind
             ;; different variables!
             ))
      ;; TODO: 
      (if (equal (var term) (var type))
          (check (body term) (body type) (acons (var term) (mk-var (var type)) env))
          (error (format nil "Limitation in typechecker: ∀ and Λ must have same
    symbol, have ~A and ~A" (var term) (var type))))))

  (:method ((term opal-struct) (type signature) env)
    (iter (for entry in (entries term))
          (for decl in (declarations type))
      (typecase entry
        (opal-definition
         (always (and (equal (var entry) (var decl))
                     (check (val entry) (ann decl) (acons (var entry) (ann decl) env)))))
        (opal-declaration
         (error "Can't deal with declarations in structures (σ)")))))

  (:method ((term term) (type opal-type) env)
    (error (format nil "Failed to typecheck term ~A as type ~A~%" term type))))


(defgeneric infer (term env)
  (:method ((term term) env)
    (error (format nil "Cannot infer type of term ~A~%" term))))
