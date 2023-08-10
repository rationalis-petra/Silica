(in-package :opal)

(defvar *opal-modules* (make-hash-table))


;; ;; bidirectional typechecking

;; (defgeneric check (term type context))
;; (defgeneric infer (term context))

;; ;; evaluation via β-reduction
(defgeneric reify (term environment)
  (:method ((term lisp-form) env)
    (form term))
  (:method ((term opal-lambda) env)
    "Reify a lambda term (λ x.e). These are converted into CL lambdas"
    `(lambda (,(var term))
       ,(reify (body term) env)))
  (:method ((term term-lambda) env)
    "Reify a lambda term (λ x.e). These are converted into CL lambdas"
    `(lambda (,(var term))
       ,(reify (body term) env)))

  (:method ((term abstract) env)
    "Reify a type forall term (Λ α.e). These are erased at runtime, and so are
equivalent to their bodies." 
    (reify (body term) env))

  (:method ((term var) env)
    "Reify a var (x). This is trivial, just extract the var's symbol"
    (var term))
  (:method ((term term-var) env)
    "Reify a term var (x). This is trivial, just extract the var's symbol"
    (var term))

  (:method ((term app) env)
    "Reify an application (e e'). An application may be either a type or term
application. If it is a type application, then we erase it. Otherwise, convert
it to a funcall."
    (typecase (right term)
      (term
       ;(format t "rhs (~A) is term (~A)!" term (type-of term))
       `(funcall ,(reify (left term) env) ,(reify (right term) env)))
      (opal-type
       ;(format t "rhs (~A) is type (~A)!" term (type-of term))
       (reify (left term) env))))


  (:method ((term opal-struct) env)
    "Reify a structure (struct (x₁ ≜ e₁) .. (xₙ ≜ eₙ)). We do this by
constructing a (let* ((x₁ e₁) .. (xₙ eₙ)) (hashmap (x₁ = e₁) .. (xₙ = eₙ)))"
    (labels
        ((mk-binds (defs)
           (iter (for def in defs)
             (when (typep def 'opal-definition)
               (collect (list (var def) (reify (val def) env))))))
         (get-vars (entries)
           (iter (for entry in entries)
             (when (typep entry 'opal-definition)
               (collect (var entry)))))
         (mk-hashmap (vars)
           ;; We can safely use a non-hygenic macro, as all opal code will only
           ;; contain symbols in the *opal-symbols* package.
           `(let ((hashmap (make-hash-table :test #'eq)))
              ,@(iter (for var in vars)
                  (collect `(setf (gethash (quote ,var) hashmap) ,var)))
              hashmap)))
      `(let*
           (,@(mk-binds (entries term)))
         ,(mk-hashmap (get-vars (entries term))))))

  (:method ((term projection) env)
    "Reify a field access (s.f). Do do this by converting it to (gethash 'f s)"
    `(gethash (quote ,(field term)) ,(reify (opal-struct term) env)))

  (:method ((term symbol) env) term)

  (:method ((term opal-literal) env) (val term))
  (:method (term env) term))
