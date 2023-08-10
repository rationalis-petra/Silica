(in-package :opal)

(defstruct env vars vals)

;; Bind var to ty in env.
(declaim (ftype (function (symbol (or opal-type kind) env) env) bind))
(defun bind (var ty env)
  (make-env
   :vars (acons var ty (env-vars env))
   :vals (acons var nil (env-vals env))))

(defparameter +empty-env+ (make-env :vars nil :vals nil))

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
    ;; iterate down the value env until we find a key
    (make-env
     :vars (env-vars env)
     :vals (build-vals var val (env-vals env)))))

(defun bind-2 (var ty val env)
  (make-env
   :vars (acons var ty (env-vars env))
   :vals (acons var val (env-vals env))))

(declaim (ftype (function (symbol env) (or null opal-type kind)) lookup))
(defun lookup (var env)
  (or (cdr (assoc var (env-vars env)))
      (error (format nil "Can't find variable ~A in environment" var))))

(defun lookup-val (var env)
  (or (cdr (assoc var (env-vals env)))
      (error (format nil "Can't find value of type ~A in environment" var))))

(defun join (env-1 env-2)
  (make-env
   :vars (append (env-vars env-1) (env-vars env-2))
   :vals (append (env-vals env-1) (env-vals env-2))))

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
    ;; Capture avoiding substiution!
    ;; (iter (for ))

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
  (let* ((vals (iter (for entry in (env-vals env))
                (when (cdr entry) (collect entry))))
         (val (ty-reduce (ty-subst type vals))))
    val))

;; α-reduce= : 
(defun α-r= (l r env)
  "Tests for α-equality in environment env. This allows substituting types for
values, to deal with existential typing."
    (α= (ty-eval l env) (ty-eval r env)))


