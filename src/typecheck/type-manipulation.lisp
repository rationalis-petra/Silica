(in-package :silica)

;; (defpackage :env
;;   (:use :cl)
;;   (:export :lookup :bind :make-from :bind-2 :join))

(defgeneric free-vars (type vars)
  (:method ((type var) vars)
    (unless (member (var type) vars) (var type)))
  (:method ((type type-var) vars)
    (unless (member (var type) vars) (var type)))
  (:method ((type tapp) vars)
    (union (free-vars (left type) vars)
           (free-vars (right type) vars)))
  (:method ((type app) vars)
    (union (free-vars (left type) vars)
           (free-vars (right type) vars)))
  (:method ((type arrow) vars)
    (union (free-vars (from type) vars)
           (free-vars (to type) vars)))
  ;; (:method ((type signature) vars)
  ;;   (union (from type) (to tapp)))
  (:method ((type forall) vars)
    (free-vars (body type) (cons (var type) vars))))


(defgeneric ty-subst (type subt)
  (:method ((type var) subst)
    ;; (if (al:lookup (var type) subst)
    ;;     (format t "lookup ~A in ~A succeeded: ~A~%" type subst (al:lookup (var type) subst))
    ;;     (format t "lookup ~A in ~A failed~%" type subst))
    (or (al:lookup (var type) subst)
        (mk-tvar (var type))))

  (:method ((type type-var) subst)
    (or (al:lookup (var type) subst)
        type))

  (:method ((type tapp) subst)
    (mk-tapp (ty-subst (left type) subst) (ty-subst (right type) subst)))

  (:method ((type app) subst)
    ;;(format t "left: ~A right: ~A~%" (ty-subst (left type) subst) (ty-subst (right type) subst))
    (mk-tapp (ty-subst (left type) subst) (ty-subst (right type) subst)))

  (:method ((type forall) subst) 
    ;; Capture avoiding substitution!
    (let ((new-var (gensym (string (var type)))))
      (if (slot-boundp type 'var-kind)
          (mk-∀
           new-var
           (var-kind type)
           (ty-subst (body type)
                     (acons (var type) (mk-tvar new-var) subst)))
          (mk-∀
           new-var
           (ty-subst (body type)
                     (acons (var type) (mk-tvar new-var) subst))))))

  (:method ((type type-lambda) subst) 
    ;; To ensure substitution avoids capture, we rename the variable bound by the ∀.
    (let ((new-var (gensym (string (var type)))))
      (if (slot-boundp type 'var-kind)
          (mk-tλ
           new-var
           (var-kind type)
           (ty-subst (body type)
                     (al:insert (var type) (mk-tvar new-var) subst)))
          (mk-tλ
           new-var
           (ty-subst (body type)
                     (al:insert (var type) (mk-tvar new-var) subst))))))

  (:method ((type inductive-type) subst) 
    ;; As with the ∀, we rename the (recursive) variable captured by the Φ.
    (let ((new-var (gensym (string (var type)))))
      (mk-induct
       new-var
       (kind type)
       (iter (for ctor in (constructors type))
         (with new-subst = (al:insert (var type) (mk-tvar new-var) subst))
         (collect
             (mk-decl
              (var ctor)
              (ty-subst (ann ctor) new-subst)))))))

  (:method ((type signature) subst) 
    (mk-sig
     (iter (for entry in (entries type))
       ;; Capture avoiding substitution!
       ;; TODO: this is WRONG!!! (how to fix??)
       ;;   + possibly two names: one which binds, one for access?
       (let ((new-var (gensym (string (var (binder entry))))))
         (collect
             (make-instance
              'entry
              :var (var entry)
              :binder 
              (mk-decl
               new-var
               (ty-subst (ann (binder entry))
                         (acons (binder entry) (mk-tvar new-var) subst)))))))))

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
  (:method ((type silica-lambda))
    (mk-tλ (var type) (var-type type)
          (ty-reduce (body type))))
  (:method ((type type-lambda))
    (mk-tλ (var type) (var-kind type)
          (ty-reduce (body type))))

  (:method ((type arrow))
    (mk-arr (ty-reduce (from type))
            (ty-reduce (to type))))


  (:method ((type app)) (ty-reduce (mk-tapp (left type) (right type))))
  (:method ((type tapp))
    (let ((left-v2 (ty-reduce (left type))))
      (typecase left-v2
        ((or type-lambda silica-lambda)
         (ty-subst (body left-v2)
                   (acons (var left-v2)
                          (ty-reduce (right type))
                          nil)))
        (t (mk-tapp left-v2 (ty-reduce (right type)))))))

  (:method ((type signature))
    (mk-sig
     (mapcar (lambda (entry)
               (mk-entry
                (var entry)
                (mk-decl (var (binder entry))
                         (ty-reduce (ann (binder entry))))))
             (entries type))))
  (:method ((type inductive-type))
    (mk-induct
     (var type)
     (kind type)
     (iter (for ctor in (constructors type))
       (collect
           (mk-decl (var ctor) (ty-reduce (ann ctor)))))))

  (:method ((type type-var)) type)
  (:method ((type var)) type)
  (:method ((type native-type)) type)
  (:method ((kind kind)) kind))

(defun ty-eval (type env)
  (let* ((vals
           (al:<>
            (iter (for (var . entry) in (env:env-locals env))
              (when (elt entry 1) (collect (cons var (elt entry 1)))))
            (iter (for (key val) in-hashtable (env:env-base env))
              (when (elt val 1) (collect (cons key (elt val 1)))))))
         (val (ty-reduce (ty-subst type vals))))
    val))

;; α-reduce= : 
(defun α-r= (l r env)
  "Tests for α-equality in environment env. This allows substituting types for
values, to deal with existential typing."
    (α= (ty-eval l env) (ty-eval r env)))

(defun β= (l r env)
  "Tests for α-equality in environment env. This allows substituting types for
values, to deal with existential typing."
    (α= (ty-eval l env) (ty-eval r env)))

(defun β<= (l r env)
  "Tests for α-equality in environment env. This allows substituting types for
values, to deal with existential typing."
    (α<= (ty-eval l env) (ty-eval r env)))

(defun β>= (l r env)
  "Tests for α-equality in environment env. This allows substituting types for
values, to deal with existential typing."
    (α>= (ty-eval l env) (ty-eval r env)))



(declaim (ftype (function (t t t t env:env) env:env) update-locals))
(defun update-locals (binder already-boundp type value local-variables)
  "Update a set of LOCAL-VARIALES by binding a VALUE of type
  TYPE. Note that this may introduce more than one variable into the
  LOCAL-VARIABLES, e.g. if VALUE is an inductive type.

For usage in modules and structures"

  (->>
   (typecase type
     ;; if current definition defines a term, check if it's been declared
     ;; if not, add it into the local bindings
     (silica-type
      (if (not already-boundp)
          (env:bind (var binder) type local-variables)
          local-variables))

     (kind
      ;; if current definition defines a type, add it's kind and
      ;; value into local bindings
      (if already-boundp
          (env:bind-existing-val (var binder) value local-variables)
          (env:bind-2 (var binder) type value local-variables)))
     (t (error "unrecognized type")))

   ((lambda (locals)
      (if (typep value 'inductive-type)
          (iter (for ctor in (constructors value))
            (accumulate
             ;; Substitute all recursive references to the inductive type with
             ;; the type itself.
             (cons
              (var ctor)
              (ty-subst (ann ctor) (al:make ((var value) . value))))
             by (lambda (c l) (env:bind (car c) (cdr c) l t))
             initial-value locals))
          locals)))))

;; (when (typep new-val 'inductive-type))
(declaim (ftype (function (symbol (or term silica-type)) (or term silica-type)) var-recur))
(defun var-recur (var value)
  "If VALUE is recursive, tell it it's name via VAR."
  (typecase value
    (inductive-type
     (setf (slot-value value 'var) var))
    (t value))
  value)
