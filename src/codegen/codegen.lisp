(in-package :sigil)

(defvar *sigil-modules* (make-hash-table))


(defgeneric reify (term context)
  (:documentation "Take a term and convert it into a piece of lisp code with the
  appropriate semantics.")
  (:method ((term lisp-form) ctx)
    (form term))
  (:method ((term sigil-lambda) ctx)
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
      (sigil-type
       (reify (left term) ctx))))


  (:method ((term sigil-struct) ctx)
    "Reify a structure (struct (x₁ ≜ e₁) .. (xₙ ≜ eₙ)). We do this by
constructing a (let* ((x₁ e₁) .. (xₙ eₙ)) (hashmap (x₁ = e₁) .. (xₙ = eₙ)))"
    (labels
        ((mk-binds (defs)
           (iter (for def in defs)
             (with current-ctx = ctx)

             (when (typep def 'sigil-definition)
               (setf current-ctx (ctx:hide (var def) current-ctx))

               (cond
                 ((or (typep (val def) 'sigil-lambda)
                      (typep (val def) 'term-lambda))
                  (accumulate 
                      (list (var def)
                            (reify-named-lambda (var def) (val def) ctx))
                      by #'cons
                      into out))

                 ((typep (val def) 'inductive-type)
                  ;; TODO: can we remove this accumulate?
                  (accumulate 
                   (list (var def) (reify (val def) ctx))
                   by #'cons
                   into out)

                  (accumulate
                  ;; Collect the defintions of all inductive types
                   (li:map #'reify-ctor (constructors (val def)))
                   by #'append
                   into out))

                 (t (accumulate
                     (list (var def) (reify (val def) ctx))
                     by #'cons
                     into out))))

             (finally (return (reverse out)))))

         (get-vars (entries)
           (iter (for entry in entries)
             (when (typep (binder entry) 'sigil-definition)
               (accumulate
                (var entry)
                by #'cons
                into out)

               (when (typep (val (binder entry)) 'inductive-type)
                 (accumulate
                  ;; Collect the defintions of all inductive types
                   (li:map #'var (constructors (val (binder entry))))
                   by #'append
                   into out)))

             (finally (return (reverse out)))))

         (mk-hashmap (vars)
           ;; We can safely use a non-hygenic macro, as all sigil code will only
           ;; contain symbols in the *sigil-symbols* package.
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
    `(gethash (quote ,(field term)) ,(reify (sigil-struct term) ctx)))

  (:method ((term conditional) ctx)
    "Reify a conditional (if e1 e2 e3). Do do this by converting it to the
common lisp (if e1 e2 e3)"
    `(if ,(reify (test term) ctx)
         ,(reify (if-true term) ctx)
         ,(reify (if-false term) ctx)))

  (:method ((term symbol) ctx)
    (if-let (val (ctx:lookup (var term) ctx))
      val
      term))

  (:method ((term sigil-literal) ctx) (val term))
  (:method (term ctx) term))

(defun fix (f)
  "Return the fixpoint of function f"
  (let ((func nil))
    (setf func (lambda (x) (funcall (funcall f func) x)))
    func))

(defun reify-named-lambda (name term ctx)
  `(fix
    (lambda (,name)
      (lambda (,(var term))
        ,(reify (body term) (ctx:hide (var term) ctx))))))

;; (defun reify-named-lambda (name term ctx)
;;   `(labels
;;     ((,name (,(var term))
;;        ,(reify (body term) (ctx:hide (var term) ctx))))))

(declaim (ftype (function (sigil-declaration) integer) calc-arity))
(defun calc-arity (ty)
  (typecase ty
    (arrow (+ 1 (calc-arity (to ty))))
    (t 0)))

(declaim (ftype (function (sigil-declaration) t) reify-ctor))
(defun reify-ctor (decl)
  (list
   (var decl)

   (let* ((arity (calc-arity (ann decl)))
          (vars (li:iota arity (lambda (x) (declare (ignore x)) (gensym)))))

     (iter (for var in vars)
       (accumulate var
        by (lambda (sym body)
             `(lambda (,sym) ,body))
        initial-value
        `(make-instance
          'sigil/impl:sigil-inductive-value
          :values (make-array ,arity :initial-contents (list ,@vars))
          :name (quote ,(var decl))))))))

