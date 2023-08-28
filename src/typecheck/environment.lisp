(defpackage :silica/environment
  (:nicknames :env)
  (:use :cl :trivia)
  (:import-from :silica
   :silica-type :kind)

  (:export
   :env :env-base :env-vals :env-vars
   :+empty+ :join
   :bind :bind-2 :bind-existing-val
   :make-from
   :lookup))
(in-package :silica/environment)

(defstruct env
  "Represents a type checking environment. Consists of three components:
 • Vars: an alist which holds the type (or kind) of locally bound variables
 • Vals: an alist which holds the type value of any variable with a kind,
   e.g. Bool = native/Boolean
 • Base: The 'base' of the environment, which is usually not modified after
   creation. Represents, e.g. a set of definitions that were imported into a
   module."
  base vars vals)


(declaim (ftype (function (symbol hash-table) (or silica-type kind)) lookup-base))
(defun lookup-base (var table)
  (let ((res (ht:lookup var table)))
    (typecase res
      (cons (car res))
      (t res))))

(declaim (ftype (function (symbol hash-table) silica-type) lookup-base))
(defun lookup-base-val (var table)
  (let ((res (ht:lookup var table)))
    (typecase res
      (cons (cdr res))
      (t res))))

;; Bind var to ty in env.
(declaim (ftype (function (symbol (or silica-type kind &optional boolean) env) env) bind))
(defun bind (var ty env &optional is-constructor)
  (make-env
   :base (env-base env)
   :vars (acons var ty (env-vars env))
   :vals (acons var nil (env-vals env))))

(defparameter +empty+ (make-env :base (ht:empty) :vars nil :vals nil))

(declaim (ftype (function (hash-table) env) make-env-from))
(defun make-from (base)
  (make-env :base base :vars nil :vals nil))

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
     :base (env-base env)
     :vars (env-vars env)
     :vals (build-vals var val (env-vals env)))))

(defun bind-2 (var ty val env)
  (make-env
   :base (env-base env)
   :vars (acons var ty (env-vars env))
   :vals (acons var val (env-vals env))))

(declaim (ftype (function (symbol env) (or null silica-type kind)) lookup))
(defun lookup (var env)
  (or (al:lookup var (env-vars env))
      (lookup-base var (env-base env))
      (error (format nil "Can't find variable ~A in environment" var))))

(defun lookup-val (var env)
  (or (al:lookup var (env-vals env))
      (lookup-base-val var (env-base env))
      (error (format nil "Can't find value of type ~A in environment" var))))

(defun join (env-1 env-2)
  (make-env
   :base (ht:merge (env-base env-1) (env-base env-2) :by
                   (lambda (x y) (declare (ignore x)) y))
   :vars (append (env-vars env-1) (env-vars env-2))
   :vals (append (env-vals env-1) (env-vals env-2))))

