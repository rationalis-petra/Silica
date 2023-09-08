(defpackage :silica/environment
  (:nicknames :env)
  (:use :cl :trivia :alexandria)
  (:import-from :silica
   :silica-type :kind)

  (:export
   :env :env-base :env-locals
   :+empty+ :join
   :bind :bind-2 :bind-existing-val
   :make-from
   :from-locals
   :lookup :lookup-ctor))
(in-package :silica/environment)


(defstruct env
  "Represents a type checking environment. Consists of three components:
 • Locals: an alist which holds the type (or kind) of locally bound variables,
   along with their value (if known) and whether or not it is a constructor (and
   thus defined a pattern). 
 • Base: The 'base' of the environment, which is usually not modified after
   creation. Represents, e.g. a set of definitions that were imported into a
   module."

  base locals)

(declaim (type env +empty+))
(defparameter +empty+ (make-env :base (ht:empty) :locals nil))
;; Local (var ↦ (ty (or val nil) is-ctor))
;; Base  (var ↦ (ty (or val nil) is-ctor))

(declaim (ftype (function (symbol env) (or null silica-type)) lookup-ctor))
(defun lookup-ctor (var env)
  (when-let ((result (or (al:lookup var (env-locals env))
                         (ht:lookup var (env-base env)))))
    (when (elt result 2)
      (elt result 0))))

(declaim (ftype (function (symbol env) (or null silica-type kind)) lookup))
(defun lookup (var env)
  (or 
   (elt
    (or (al:lookup var (env-locals env))
        (ht:lookup var (env-base env)))
    0)
   (error (format nil "Can't find variable ~A in environment" var))))

;; (declaim (ftype (function () (or silica-type term)) lookup-val))
(defun lookup-val (var env)
  (or (elt (al:lookup var (env-locals env)) 1)
      (elt (ht:lookup var (env-base env)) 1)
      (error (format nil "Can't find value of type ~A in environment" var))))

;; Bind var to ty in env.
(declaim (ftype (function (symbol (or silica-type kind) env &optional boolean) env) bind))
(defun bind (var ty env &optional is-constructor)
  (make-env
   :base (env-base env)
   :locals (acons var (list ty nil is-constructor) (env-locals env))))

(declaim (ftype (function (symbol kind silica-type env &optional boolean) env) bind-2))
(defun bind-2 (var ty val env &optional is-constructor)
  (make-env
   :base (env-base env)
   :locals (acons var (list ty val is-constructor) (env-locals env))))

(declaim (ftype (function (hash-table) env) make-env-from))
(defun make-from (base)
  (make-env :base base :locals nil))

(declaim (ftype (function (list) env) from-locals))
(defun from-locals (locals)
  (make-env :base (ht:empty) :locals locals))

;; Bind var to val in env.
;; Mostly for, e.g. looking up types in the environment. 
(defun bind-existing-val (var val env)
  (labels ((build-locals (var val locals)
             (match locals
               ((guard (cons (cons nvar (list ty _ ctor)) rest)
                       (eq var nvar))
                (acons var (list ty val ctor) rest))
               ((cons (cons nvar ninfo) rest)
                (acons nvar ninfo (build-locals var val rest)))
               (nil (error "Tried to insert val for non-bound var ~A" var)))))
    ;; iterate down the value env until we find a key
    (make-env
     :base (env-base env)
     :locals (build-locals var val (env-locals env)))))

(defun join (env-1 env-2)
  (make-env
   :base (ht:merge (env-base env-1) (env-base env-2) :by
                   (lambda (x y) (declare (ignore x)) y))
   :locals (append (env-locals env-1) (env-locals env-2))))

