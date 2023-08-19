(in-package :opal)



;; Module/Package system
;; Inspired by Ocaml

(defvar *packages* (ht:empty))

(defclass opal-package ()
  ((name
    :type string
    :reader name
    :initarg :name)
   (modules
    :type hash-table
    :reader modules
    :initarg :modules
    :initform (ht:empty))
   (exported-modules
    :type list
    :reader exported-modules
    :initarg :exported-modules
    :initform nil)
   (dependenceis
    :type list
    :reader dependencies
    :initarg :dependencies
    :initform nil))
  (:documentation "The Package class represents a d"))


(defclass module ()
  ((parent
    :reader parent
    :initarg :parent
    :initform nil)
   (name
    :type string
    :reader name
    :initarg :name)
   (source
    :type string
    :reader source
    :initarg :source
    :initform :unknown)
   (dependencies
    :type list
    :reader dependencies
    :initarg :dependencies)
   (internal-struct
    :type list 
    :reader internal-struct
    :initarg :internal-struct)
   (module-package
    :type package
    :reader module-package
    :initarg :package)
   (submodules
    :type list
    :reader submodules
    :initarg :submodules
    :initform nil)
   (exports
    :type list
    :reader exports
    :initarg :exports
    :initform (ht:empty)))
  (:documentation "The Module class represents a discrete unit of code within
the opal programming Language. At the language-level, modules look identical
to structures.

However, they require distinct implementations, to facilitate build-systems and
modular recompilation. "))


;; All packages currently loaded

(declaim (ftype (function (t) opal-package) get-package))
(defun get-package (name)
  (gethash name *packages*))

(declaim (ftype (function (opal-package) hash-table) get-exports))
(defun get-exports (package)
  "Get a list of exported modules from a package"
  (iter (for name in (exported-modules package))
    (with exports = (ht:empty))
    (setf (gethash name exports)
          (lookup name (modules package)))

    (finally (return exports))))

(declaim (ftype (function (opal-package) hash-table) get-available-modules))
(defun get-available-modules (package)
  "Given a package, get a list of modules which should be available for import
in all it's modules."
  (iter (for pkg-name in (dependencies package))
    (accumulate
     (get-exports (get-package pkg-name))
     by (lambda (t1 t2)
          (ht:merge t1 t2
                    :by (alexandria:curry #'list :ambiguous-name)))
     :initial-value (ht:empty))))

(defun build-module (package module-raw)
  (labels
      ((parse-module (module-raw)
         (al:insert
          :ast
          (iter (for entry in (al:lookup :body module-raw))
            (collect 
                (-> entry (infixify) (to-def))))
          module-raw))

       (type-module (module-ast env)
         (let ((result
                 (infer
                  (mk-struct
                   (li:map (lambda (def)
                             (mk-entry (var def) def))
                           (al:lookup :ast module-ast)))
                  env)))
           (-> module-ast
            (al:insert :type (car result))
            (al:insert :typed-ast (cdr result)))))

       (reify-module (module env)
           (reify (lookup :typed-ast module) env)))

    (let* (;; TODO: also account for modules at the level of the package!
           (modules (get-available-modules package))

           ;; Generate typechecking environment from imported modules + module
           ;; definition.
           (env (gen-env modules module-raw))

           ;; TODO add parser fixity generation
           )

      ;; now, start program generation 
      (-> module-raw
          (parse-module)
          (type-module env)
          (reify-module env)))))


(defun gen-env (available-modules module-raw)
  (let ((imports (al:lookup :import-list module-raw)))
    ;; for now, assume imports are a list of symbols which designate packages
    (iter (for name in imports)

      (when (not (gethash name available-modules))
        (error (format nil "Couldn't find module of name ~A" name)))
      (accumulate 
       (gethash name available-modules)
       by
       (lambda (env1 env2)
         (ht:merge
          env1 env2
          :by (alexandria:curry #'list :ambiguous-name)))
       into base)

      (finally (return (make-env-from base))))))
;; (defun resolve-import (import-qualifier))


;; (defun generate-env (import-qualifiers module)
;;   "Generate a typechecking environment from a module"
;;   t)


