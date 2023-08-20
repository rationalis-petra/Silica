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
    :type (or string (eql :unknown))
    :reader source
    :initarg :source
    :initform :unknown)
   (dependencies
    :type list
    :reader dependencies
    :initarg :dependencies)
   (signature
    :type t
    :reader signature
    :initarg :signature)
   (internal-struct
    ;:type list 
    :reader internal-struct
    :initarg :internal-struct)
   (lisp-val
    :type t
    :reader lisp-val
    :initarg :lisp-val)
   (module-package
    :type package
    :reader module-package
    :initarg :module-package)
   (submodules
    :type list
    :reader submodules
    :initarg :submodules
    :initform nil)
   (exports
    :type list
    :reader exports
    :initarg :exports
    :initform nil))
  (:documentation "The Module class represents a discrete unit of code within
the opal programming Language. At the language-level, modules look identical
to structures.

However, they require distinct implementations, to facilitate build-systems and
modular recompilation. "))


;; All packages currently loaded
;; Functions to manipulate packages and modules

(declaim (ftype (function (t) opal-package) get-package))
(defun get-package (name)
  (gethash name *packages*))

(declaim (ftype (function (opal-package) hash-table) get-exports))
(defun get-exports (package)
  "Get a list of exported modules from a package"
  (iter (for name in (exported-modules package))
    (with exports = (ht:empty))
    (setf (gethash name exports)
          (ht:lookup name (modules package)))

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



;; Functions to get information from 'raw' (read) syntax trees.
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
           (->> module-ast
            (al:insert :type (car result))
            (al:insert :typed-ast (cdr result)))))

       (reify-module (module env)
         (al:insert 
          :code
          (reify (al:lookup :typed-ast module) env)
    s      module)))

    (let* (;; TODO: also account for modules at the level of the package!
           (modules (get-available-modules package))

           ;; Generate typechecking environment from imported modules + module
           ;; definition.
           (env (gen-env modules module-raw))

           ;; TODO add parser fixity generation
           )

      (-> module-raw
          (parse-module)
          (type-module env)
          (reify-module env)))))

(declaim (ftype (function (hash-table list) env) gen-env))
(defun gen-env (available-modules module-raw)
  (let ((imports (al:lookup :import-list module-raw)))
    ;; for now, assume imports are a list of symbols which designate packages
    (iter (for name in imports)

      (when (not (gethash name available-modules))
        (error (format nil "Couldn't find module of name ~A" name)))
      
      (accumulate 
       (module-export-types (gethash name available-modules))
       by
       (lambda (table-1 table-2)
         (format t "table-1: ~A~%table-2: ~A~%" table-1 table-2)
         (if table-2
             (ht:merge
              table-1 table-2
              :by (alexandria:curry #'list :ambiguous-name))
             table-1))
       into imported-modules)

      (finally (return (make-env-from imported-modules))))))


(defun module-export-types (module) ;; TODO: add filter as an argument
  ;; get exports from a module as a list of symbols

  (iter (for symbol in (exports module))
    (with out = (ht:empty))

    (let ((tipe (get-field (signature module) symbol)))
      (typecase tipe
        (kind (setf (gethash symbol out)
                    (cons tipe (get-field (internal-struct module) symbol))))
        (t (setf (gethash symbol out) tipe))))

    (finally (return out))))



