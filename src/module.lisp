(in-package :opal)



;; Module/Package system
;; Inspired by Ocaml

;; (λ x y. p﹨q)

(defclass opal-package ()
  ((name
    :type string
    :reader name
    :initarg :name)
   (modules
    :type hash-table
    :reader modules)
   (exported-modules
    :type list
    :reader exported-modules)
   (dependenceis
    :type list
    :reader dependencies))
  (:documentation "The Package class represents a d "))


(defclass module ()
  ((parent
    :reader parent
    :initarg :parent)
   (name
    :type string
    :reader name
    :initarg :name)
   (source
    :type string
    :reader source
    :initarg :source)
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
    :initarg :submodules)
   (exports
    :type list
    :reader exports
    :initarg :exports))
  (:documentation "The Module class represents a discrete unit of code within
the opal programming Language. At the language-level, modules look identical
to structures.

However, they require distinct implementations, to facilitate build-systems and
modular recompilation. "))


;; All packages currently loaded
(defvar *packages* (make-hash-table :test #'equal))

(declaim (ftype (function (string) opal-package) get-package))
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
                    :by (alexandria:curry #'list :merge-conflict))))))

(defun build-module (package module-raw)
  (labels
      ((parse-module (module-raw)
         (al:insert
          :ast
          (iter (for entry in (lookup :sexpr module-raw))
            (collect 
                (-> entry (infixify) (to-def))))
          module-raw))

       (check-module (module-ast env)
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
          (check-module env)
          (reify-module env)))))


(defun gen-env (available-modules module-raw))
;; (defun resolve-import (import-qualifier))


;; (defun generate-env (import-qualifiers module)
;;   "Generate a typechecking environment from a module"
;;   t)


