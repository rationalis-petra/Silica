(in-package :silica)

(defvar *silica-modules* (make-hash-table))


(defgeneric reify (term context)
  (:documentation "Take a term and convert it into a piece of lisp code with the
  appropriate semantics.")
  (:method ((term lisp-form) ctx)
    (form term))
  (:method ((term silica-lambda) ctx)
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
      (silica-type
       (reify (left term) ctx))))


  (:method ((term silica-struct) ctx)
    "Reify a structure (struct (x₁ ≜ e₁) .. (xₙ ≜ eₙ)). We do this by
constructing a (let* ((x₁ e₁) .. (xₙ eₙ)) (hashmap (x₁ = e₁) .. (xₙ = eₙ)))"
    (labels
        ((mk-binds (defs)
           (iter (for def in defs)
             (with current-ctx = ctx)

             (when (typep def 'silica-definition)
               (setf current-ctx (ctx:hide (var def) current-ctx))

               (let ((inner-val (get-inner-val (val def))))
                 (cond
                   ((or (typep inner-val 'silica-lambda)
                        (typep inner-val 'term-lambda))
                    (accumulate 
                     (list (var def)
                           (reify-named-lambda (var def) inner-val ctx))
                     by #'cons
                     into out))

                   ((typep (val def) 'inductive-type)
                    ;; TODO: can we remove this accumulate?
                    (accumulate 
                     (list (var def) (reify inner-val ctx))
                     by #'cons
                     into out)

                    (accumulate
                     ;; Collect the defintions of all inductive types
                     (li:map #'reify-ctor (constructors inner-val))
                     by #'append
                     into out))

                   (t (accumulate
                       (list (var def) (reify inner-val ctx))
                       by #'cons
                       into out)))))

             (finally (return (reverse out)))))

         (get-vars (entries)
           (iter (for entry in entries)
             (when (typep (binder entry) 'silica-definition)
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
           ;; We can safely use a non-hygenic macro, as all silica code will only
           ;; contain symbols in the *silica-symbols* package.
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
    `(gethash (quote ,(field term)) ,(reify (silica-struct term) ctx)))

  (:method ((term pattern-match) ctx)
    "Reify a pattern-match φ e ((pat₀ → bdy₀) ... (patₙ → bdyₙ)). Do do this by
converting it to a cond whose cases test for the appropriate pattern. If
a cond matches, then use a let to get the appropriate variables."

    (labels 
        ((destructure (term clause)
           (let ((pattern (remove-tvars (pattern clause))))
             (list
              (reify-pattern-test term pattern)
              `(let* ,(reify-pattern-bind term pattern)
                 ,(reify (body clause)
                         (reduce (lambda (x y) (ctx:hide y x)) (pattern-vars pattern)
                                         :initial-value ctx))))))

         (remove-tvars (pattern)
           (match pattern
             ((pair (fst :var) (snd _)) pattern)
             ((pair (fst :tvar) (snd _)) nil)
             ((cons pr tail)
              (cons pr
                    (remove-if (curry #'eq nil)
                               (mapcar #'remove-tvars tail))))))

         ;; Given a pattern, generate an expression which will evaluate to true
         ;; if a value matches that pattern.
         (reify-pattern-test (term pattern)
           (match pattern
             ((pair (fst :var) (snd _)) t)
             ((cons (pair (fst :destruct) (snd sym)) tail)
              `(and (eq (silica/impl:name ,term) (quote ,sym))
                    ,@(iter (for subpattern in tail)
                        (for i from 0)
                        (collect (reify-pattern-test
                                  `(elt (silica/impl:induct-values ,term) ,i)
                                  subpattern)))))))

         ;; Assume a pattern has matched, create clauses in a let* binding
         (reify-pattern-bind (term pattern)
           (match pattern
             ((pair (fst :var) (snd sym))
              (list (list sym term)))
             ((cons (pair (fst :destruct) (snd _)) tail)
              (iter (for subpattern in tail)
                    (for i from 0)
                (let ((subpattern-name (gensym)))
                  (collect (list subpattern-name
                                 `(elt (silica/impl:induct-values ,term) ,i))
                    into subpattern-names)
                  (collect (reify-pattern-bind subpattern-name subpattern)
                    into subpattern-bindings)

                  (finally (return
                             (li:<> subpattern-names
                                    (li:join subpattern-bindings)))))))))

         ;; Given a pattern, return a list of all variables it binds
         (pattern-vars (pattern)
           (match pattern
             ((pair (fst :var) (snd sym)) (list sym))
             ((cons (pair (fst :destruct) (snd _)) tail)
              (apply #'append (mapcar #'pattern-vars tail))))))


      (let ((term-name (gensym "destructure")))
        `(let ((,term-name ,(reify (term term) ctx)))
           (cond
             ,@(mapcar (curry #'destructure term-name) (clauses term))
             (t (error (format nil "failed to match value ~A~%" ,term-name))))))))

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

  (:method ((term silica-literal) ctx) (val term))
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

(declaim (ftype (function (silica-type) integer) calc-arity))
(defun calc-arity (ty)
  (typecase ty
    (arrow (+ 1 (calc-arity (to ty))))
    (forall (calc-arity (body ty)))
    (t 0)))

(declaim (ftype (function (silica-declaration) t) reify-ctor))
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
          'silica/impl:silica-inductive-value
          :values (make-array ,arity :initial-contents (list ,@vars))
          :name (quote ,(var decl))))))))


(defun get-inner-val (val)
  (typecase val
    (abstract (get-inner-val (body val)))
    (t val)))
