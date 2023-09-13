(in-package :silica)


(define-condition unexpected-type (error)
  ((message
    :type string
    :initarg :message
    :initform ""
    :reader message)
   (actual-type
    :type silica-type
    :initarg :actual-type
    :reader actual-type)
   (expected-type
    :type silica-type
    :initarg :actual-type
    :reader message))
  (:documentation "For when the declared type does not match the actual type of
  an expression"))


;;(declaim (ftype (function (silica-type) list) type-to-list))
(defun type-to-list (type)
  (typecase type
    (arrow (cons (cons (from type) nil) (type-to-list (to type))))
    (forall (cons (cons (var-kind type) (mk-tvar (var type)))
                  (type-to-list (body type))))
    (t (list (cons type nil)))))

(declaim (ftype (function ((or term silica-type)) list) unroll-type))
(defun unroll-type (type)
  (typecase type
    (tapp (cons (left type) (unroll-type (right type))))
    (app  (cons (left type) (unroll-type (right type))))
    (t (list type))))

;; TODO: return information relating to the expected type of the body, e.g.
;; if we have a Expr α but this pattern forces α=Bool, then return that information
;; i.e. return also a substitution to be applied to the return type
(declaim (ftype (function ((or list symbol) (or kind silica-type) env:env) list) extract-pattern))
(defun extract-pattern (pattern type env)
  "Given a PATTERN which is matching a value of type TYPE in an ENVIRONMENT,
return the pair representing:
1. An updated representation of the pattern. 
2. A set of local variables, representing variables which are bound within the
   pattern."

  (labels
       ((extract-subpattern (pattern ptn-type value env)
         (match pattern
           ((guard (list 'sym::|⟨⟩| s)
                   (typep s 'symbol))
            (assert (typep ptn-type 'kind))
            (list (pair :tvar s)
                  (al:make
                   (s . (list ptn-type value nil)))
                  (al:empty)))
           (_ (extract-pattern pattern ptn-type env))))

        (mk-subst (from to)
          (match from
            ((tapp (left l) (right r))
             (if (typep r 'type-var)
                 (al:insert (var r) (right to) (mk-subst l (left to)))
                 (mk-subst l (left to))))
            (t (al:empty)))))

    (typecase pattern  
      (list
       (let ((head (first pattern))
             (tail (rest pattern)))
         (if-let ((pattern-type (env:lookup-ctor head env)))
           (progn 

             (unless (= (length pattern) (length (type-to-list pattern-type)))
               (error
                (format nil "Bad pattern: pattern ~A has ~A elements, but type
~A has ~A elements" pattern (length tail) pattern-type (length (type-to-list pattern-type)))))


             (iter (for subpattern in tail)
               (for (var-type . var-val) in (type-to-list pattern-type))

               (with subst = (mk-subst type (caar (last (type-to-list pattern-type)))))

               (match (extract-subpattern subpattern var-type var-val env)
                 ((list new-subpattern new-locals new-subst)

                  (collect new-locals into local-set)
                  (collect new-subpattern into subpatterns)
                  (collect new-subst into substitutions)))

               (finally
                (return
                  (list
                   (cons (pair :destruct head) subpatterns)
                   (li:join local-set)
                   (al:<> subst (al:join substitutions)))))))

           (if tail
               (error (format nil "Unrecognized pattern: ~A" pattern))
               (list
                (pair (if (typep type 'kind) :tvar :var) head)
                (al:make (head . (list type nil nil)))
                (al:empty))))))
      ;; TODO: substitution?
      (symbol
       (list
        (pair (if (typep type 'kind) :tvar :var) pattern)
        (al:make (pattern . (list type nil nil)))
        (al:empty))))))

;; we use bidirectional typechecking
;; i.e. 1x check method, 1x infer method
(declaim (ftype (function (t t t) (or term silica-type)) check))
(defgeneric check (term type env)
  (:documentation "Perform typechecking.")

  (:method ((term lisp-form) type env)
    (if (β<= (form-type term) type env)
        term
        (error 'unexpected-type
               :expected-type type
               :actual-type (form-type term))))

  (:method ((term var) type env)
    (if (β<= type (env:lookup (var term) env) env)
        (typecase type
          (kind (ty-eval (mk-tvar (var term)) env))
          (silica-type (mk-mvar (var term))))
        (error 'unexpected-type
               :expected-type type
               :actual-type (env:lookup (var term) env))))

  (:method ((term term-var) type env)
    (if (β<= type (env:lookup (var term) env) env)
        term
        (error (format nil "Var ~A does not have type ~A, but rather~A~%"
                       (var term) type (env:lookup (var term) env)))))

  (:method ((term type-var) type env)
    (if (β<= type (env:lookup (var term) env) env)
        term
        (error (format nil "Var ~A does not have type ~A, but rather~A~%"
                       (var term) type (env:lookup (var term) env)))))

  (:method ((term silica-literal) (type native-type) env)
    (if (typep (val term) (native-type type))
        term
        (error (format nil "Term ~A does not have type ~A~%" term type))))

  (:method ((term silica-lambda) (type arrow) env)
    (flet ((body-check (from to)
             (check (body term) to (env:bind (var term) from env))))
      (if (slot-boundp term 'var-type)
          (if (α-r= (from type) (check (var-type term) (mk-kind) env) env)
              (mk-mλ (var term) (from type) (body-check (var-type term) (to type)))
              (error "Declared function argument type doesn't match actual type"))
          (mk-mλ (var term)
                (from type)
                (body-check (from type) (to type))))))


  (:method ((term abstract) (type forall) env)
    (flet ((body-check (body type var1 var2 kind)
             (if (eq var1 var2)
                 (check body type (env:bind var2 kind env))
                 (check body
                        (ty-subst type (acons var1 (mk-tvar var2) nil))
                        (env:bind var2 kind env)))))
      ;; TODO: 
      (unless (or (not (slot-boundp term 'var-kind))
                  ;; (not (slot-boundp type 'var-kind))
                  (α= (var-kind term) (var-kind type)))
        (error (format nil "Type check failed due to kind mismatch")))
      
      (mk-abs (var term) (var-kind type)
              (body-check (body term) (body type) (var type) (var term) (var-kind type)))))

  (:method ((term silica-struct) (type signature) env)
    (labels ((has-repeating-field (list)
               (cond
                 ((null list) nil)
                 ((member (car list) (cdr list)) t)
                 (t (has-repeating-field (cdr list))))))

      ;; If it has repeating fields, typecheck fails
      (when (has-repeating-field
             (iter (for entry in (entries term))
               (when (typep (binder entry) 'silica-definition)
                 (collect (var entry)))))
        (error "Repeating definitions in term: ~A" term))

      (mk-struct
       (iter (for entry in (entries term))
             (for binder = (binder entry))
             (with decls = (entries type))
             (with locals = env:+empty+)
             (with prev-decl = nil)

         ;; locals = local declarations
         ;; (with type-vals = nil)
         (typecase binder
           (silica-definition
            (let* ((decl-entry (or prev-decl (pop decls)))
                   (new-val
                     (check
                      (val binder)
                      (ty-eval (ann (binder decl-entry)) (env:join locals env))
                      (env:join locals env))))

              (unless (eq (var entry) (var decl-entry))
                (error "Definition variable not equal to declaration variable: ~A ~A"
                       (var entry) (var decl-entry)))


              ;; Update locals
              (typecase new-val
                (term
                 ;; if current definition defines a term, check if it's been declared
                 ;; if not, add it into the locals
                 (unless prev-decl
                   (setf locals (env:bind (var (binder decl-entry)) (ann (binder decl-entry)) locals))))
                (silica-type
                 ;; if current definition defines a type, add it's kind and
                 ;; value into local
                 ; (setf locals binder prev-decl new-val locals)
                 (if prev-decl
                     (setf locals (env:bind-existing-val (var (binder decl-entry)) new-val locals))
                     (setf locals (env:bind-2 (var (binder decl-entry)) (ann (binder decl-entry)) new-val locals)))))
              (setf prev-decl nil)

              (collect decl-entry)
              (collect (mk-entry (var entry) (mk-def (var entry) new-val)))))
           (silica-declaration
            (let ((decl (pop decls)))
              (when prev-decl
                (error "Can't have two declarations in a row"))
              (unless (α-r= entry decl env) ;; TODO: substitute value!
                (error "Can't deal with declarations in structures (σ)"))
              (setf prev-decl decl)
              (setf locals (env:bind (var (binder decl)) (ann (binder decl)) locals)))))))))

  (:method ((term conditional) (type silica-type) env)
    (let* ((test (check (test term) (mk-native 'boolean) env))
           (if-true (check (if-true term) type env))
           (if-false (check (if-false term) type env)))
      (mk-if test if-true if-false)))

  (:method ((term projection) (type silica-type) env)
    (let* ((struct-result (infer (silica-struct term) env))
           (struct-ty (car struct-result))
           (struct-val (cdr struct-result)))
      (if (α-r= (get-field struct-ty (field term)) type env)
          (mk-proj (field term) struct-val)
          (error "bad projection"))))

  ;; TODO: check type vs term application??
  (:method ((term app) type env)
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
             ;; TODO: possibly TY-eval (right term)?
             (mk-app lv (check (right term) (ty-eval (from lt) env) env))
             (error "Bad application of type constructor")))

        (forall
         ;; Assume kind is τ if not provided (TODO: kind inference!)
         (let* ((kind (if (slot-boundp lt 'var-kind)
                         (var-kind lt)
                         (mk-kind)))

                ;; Check that rhs has the expected kind 
                (right-val (check (ty-eval (right term) env) kind env)))

           ;; check that applying r to l gives the type we expect
           (unless (β<= (ty-subst (body lt) (acons (var lt) right-val nil))
                        type
                        env)
             (error "Bad application of abstraction"))

           (mk-tapp lv right-val)))
        (t (error "Applying to neither function or abstraction")))))

  (:method ((term pattern-match) (type silica-type) env)
    (destructuring-bind (val-ty . value)
        (infer (term term) env)

      ;; Check patterns
      (mk-match
       value
       (iter (for clause in (clauses term))

         ;; First, get pattern variables
         (match (extract-pattern (pattern clause) val-ty env)
           ((list new-pattern locals subst)
            ;; second, check body
            (collect
                (make-instance
                 'match-clause
                 :pattern new-pattern
                 :body (check (body clause) (ty-subst type subst) (env:join (env:from-locals locals) env))))))))))

  ;; Kind Checking
  (:method ((type native-type) (kind kind-type) env) type)

  (:method ((type arrow) (kind kind-type) env)
    (mk-arr (check (from type) kind env) (check (to type) kind env)))

  (:method ((type signature) (kind kind-type) env)
    ;; TODO: iterate through & check for well-formedness
    type)

  (:method ((type silica-type) (kind kind-is) env)
    (let ((new-type (ty-eval type env)))
      (if (α<= type (silica-type kind))
          new-type
          (error (format nil "Type ~A not equal to value declared by is-kind: ~A" type kind)))))

  (:method ((term silica-lambda) (type kind-arrow) env)
    (flet ((body-check (from to)
             (check (body term) to (env:bind (var term) from env))))
      (if (slot-boundp term 'var-type)
          (if (α= (from type) (var-type term))
              (body-check (var-type term) (to type))
              (error "Declared type constructor argument kind doesn't match actual kind"))
          (mk-tλ (var term)
                 (from type)
                 (body-check (from type) (to type))))))

  (:method ((type inductive-type) (kind kind) env)
    ;; Iterate through all constructors
    (mk-induct
     (var type)
     (if-slot (vk type 'kind) vk kind)
     (iter (for ctor in (constructors type))
       (collect
           (mk-decl
            (var ctor)
            (check (ann ctor) (mk-kind) env))))))


  (:method ((type forall) (kind kind-type) env)
    (let ((kind (if-slot (vkind type 'var-kind) vkind (mk-kind))))
     (mk-∀ (var type) kind
           (check (body type) (mk-kind) (env:bind (var type) kind env)))))

  ;; Failure cases
  (:method ((term term) (type silica-type) env)
    (error (format nil "Failed to typecheck term ~A as type ~A~%" term type)))

  (:method ((type silica-type) (kind kind) env)
    (error (format nil "Failed to typecheck type ~A as kind ~A~%" type kind)))

  (:method ((type-1 silica-type) (type-2 silica-type) env)
    (error (format nil "Failed to typecheck type ~A as type ~A~%" type-1 type-2)))

  (:method ((term term) (kind kind) env)
    (error (format nil "Failed to typecheck term ~A as kind ~A~%. Note: Terms to
    not have kinds!" term kind))))


(declaim (ftype (function (t t) (or (cons term silica-type) (cons silica-type kind))) infer))
(defgeneric infer (term env)

  (:method ((term lisp-form) env)
    (cons (ty-eval (form-type term) env)
        term))

  (:method ((term silica-literal) env)
    (cons 
     (mk-native
      ;; TODO: formalize somewhere the default inference/correspondence
      ;; lisp type ↔ silica type
      (type-of (val term)))
     term))

  (:method ((term var) env)
    (let ((ty (env:lookup (var term) env)))
      (if ty
          (cons ty
                (typecase ty
                  (kind (mk-tvar (var term)))
                  (silica-type (mk-mvar (var term)))))
          (error (format nil "No type found in environment for variable ~A"
                         (var term))))))
  
  (:method ((term type-var) env)
    (let ((ty (env:lookup (var term) env)))
      (if ty
          (cons ty term)
          (error (format nil "No type found in environment for variable ~A"
                         (var term))))))

  (:method ((term term-var) env)
    (let ((ty (env:lookup (var term) env)))
      (if ty
          (cons ty term)
          (error (format nil "No type found in environment for variable ~A"
                         (var term))))))

  (:method ((term silica-lambda) env)
    (if (slot-boundp term 'var-type)
        (let* ((arg-ty (if (typep (var-type term) 'kind)
                           (ty-eval (var-type term) env)
                           (check (ty-eval (var-type term) env) (mk-kind) env)))
               (body-result (infer (body term)
                                   (env:bind (var term) arg-ty env))))
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
                               (env:bind (var term) kind env))))
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
             (cons (ty-eval (to lt) env) (mk-app lv rv))
             (error "Bad application: term of type ~A cannot be applied to term of type ~A" rt lt)))
        (kind-arrow
         (if (β>= (from lt) rt env)
             (cons (ty-eval (to lt) env) (mk-tapp lv rv))
             (error "Bad application: type of kind ~A cannot be applied to type
           of kind ~A" rt lt)))
        (forall
         (if (β>= (var-kind lt) rt env)
             (cons 
              (ty-eval (ty-subst (body lt) (acons (var lt) (right term) nil)) env)
              (mk-app lv (ty-eval rv env)))
             (error "Bad application of abstraction: mismatched kinds ~A and ~A"
                    (var-kind lt) rt)))
        (t (error "Applying to neither function or abstraction of type: ~A" (show lt))))))

  (:method ((term silica-struct) env)
    (labels ((has-repeating-field (list)
               (cond
                 ((null list) nil)
                 ((member (car list) (cdr list)) t)
                 (t (has-repeating-field (cdr list))))))

      ;; If it has repeating fields, typecheck fails
      (when (has-repeating-field
             (iter (for entry in (entries term))
               (when (typep (binder entry) 'silica-definition)
                 (collect (var entry)))))
        (error "Repeating definitions in term: ~A" term))

      (iter (for entry in (entries term))
        (for binder = (binder entry))
        ;; locals = local declarations
        (with locals = env:+empty+)
        (with prev-decl = nil)
        (typecase binder
          (silica-definition
           (let* ((new-entry
                    (if prev-decl 
                        (let ((ty (ty-eval (ann (binder prev-decl)) (env:join locals env))))
                          (cons ty
                                (check (var-recur (var binder) (val binder))
                                       ty
                                       (env:join locals env))))
                        (infer
                         (val (binder entry))
                         (env:join locals env))))
                  (new-ty (car new-entry))
                  (new-val (cdr new-entry)))

             ;; TODO: account for renaming?
             (unless (or (not prev-decl) (eq (var entry) (var prev-decl)))
               (error "Definition variable not equal to declaration variable: ~A ~A"
                      (var entry) (var prev-decl)))


             ;; Update locals
             (setf locals (update-locals binder prev-decl new-ty new-val locals))

             (collect (or (when prev-decl (binder prev-decl))
                          (mk-decl (var binder) new-ty))
               into out-decls)

             (collect (or (when prev-decl (binder prev-decl))
                          (mk-decl (var binder) new-ty))
               into out-entries)
             (collect (mk-def (var binder) new-val) into out-entries)
             (setf prev-decl nil)))
          (silica-declaration
           (setf prev-decl entry)
           (setf locals (env:bind (var binder) (ty-eval (ann binder) (env:join locals env)) locals))))
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
    (let* ((struct-result (infer (silica-struct term) env))
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
                (infer (body type) (env:bind (var type) (var-kind type) env))
                (infer (body type) (env:bind (var type) (mk-kind) env))))
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
