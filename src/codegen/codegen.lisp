(in-package :opal)

(defvar *opal-modules* (make-hash-table))

(defgeneric reify (term context)
  (:documentation "Take a term and convert it into a piece of lisp code with the
  appropriate semantics.")
  (:method ((term lisp-form) ctx)
    (form term))
  (:method ((term opal-lambda) ctx)
    "Reify a lambda term (λ x.e). These are converted into CL lambdas"
    `(lambda (,(var term))
       ,(reify (body term) (ctx:hide (var term) ctx))))
  (:method ((term term-lambda) ctx)
    "Reify a lambda term (λ x.e). These are converted into CL lambdas"
    `(lambda (,(var term))
       ,(reify (body term) (ctx:hide (var term) ctx))))

  (:method ((term abstract) ctx)
    "Reify a type forall term (Λ α.e). These are erased at runtime, and so are
equivalent to their bodies." 
    (reify (body term) (ctx:hide (var term) ctx)))

  (:method ((term var) ctx)
    "Reify a var (x). This is trivial, just extract the var's symbol"
    (or (ctx:lookup (var term) ctx) (var term)))
  (:method ((term term-var) ctx)
    "Reify a term var (x). This is trivial, just extract the var's symbol"
    (or (ctx:lookup (var term) ctx) (var term)))

  (:method ((term app) ctx)
    "Reify an application (e e'). An application may be either a type or term
application. If it is a type application, then we erase it. Otherwise, convert
it to a funcall."
    (typecase (right term)
      (term
       `(funcall ,(reify (left term) ctx) ,(reify (right term) ctx)))
      (opal-type
       (reify (left term) ctx))))


  (:method ((term opal-struct) ctx)
    "Reify a structure (struct (x₁ ≜ e₁) .. (xₙ ≜ eₙ)). We do this by
constructing a (let* ((x₁ e₁) .. (xₙ eₙ)) (hashmap (x₁ = e₁) .. (xₙ = eₙ)))"
    (labels
        ((mk-binds (defs)
           (iter (for def in defs)
             (when (typep def 'opal-definition)
                   (with current-ctx = ctx)

               (setf current-ctx (ctx:hide (var def) current-ctx))
               (collect (list (var def) (reify (val def) current-ctx))))))
         (get-vars (entries)
           (iter (for entry in entries)
             (when (typep (binder entry) 'opal-definition)
               (collect (var entry)))))
         (mk-hashmap (vars)
           ;; We can safely use a non-hygenic macro, as all opal code will only
           ;; contain symbols in the *opal-symbols* package.
           `(let ((hashmap (make-hash-table :test #'eq)))
              ,@(iter (for var in vars)
                  (collect `(setf (gethash (quote ,var) hashmap) ,var)))
              hashmap)))
      `(let*
           ;; TODO: sort out differing variable names within/without?
           (,@(mk-binds (li:map #'binder (entries term))))
         ,(mk-hashmap (get-vars (entries term))))))

  (:method ((term projection) ctx)
    "Reify a field access (s.f). Do do this by converting it to (gethash 'f s)"
    `(gethash (quote ,(field term)) ,(reify (opal-struct term) ctx)))

  (:method ((term symbol) ctx)
    (if-let (val (ctx:lookup (var term) ctx))
      val
      term))

  (:method ((term opal-literal) ctx) (val term))
  (:method (term ctx) term))
