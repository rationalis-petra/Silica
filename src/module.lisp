(in-package :silica)

;; Module/Package system
;; Inspired by Ocaml

(defvar *packages* (ht:empty))

(defclass silica-package ()
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
   (dependencies
    :type list
    :reader dependencies
    :initarg :dependencies
    :initform nil))
  (:documentation "The Package class represents a collection of modules"))


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
   ;; (submodules
   ;;  :type list
   ;;  :reader submodules
   ;;  :initarg :submodules
   ;;  :initform nil)
   (exports
    :type list
    :reader exports
    :initarg :exports
    :initform nil))
  (:documentation "The Module class represents a discrete unit of code within
the silica programming Language. At the language-level, modules look identical
to structures.

However, they require distinct implementations, to facilitate build-systems and
modular recompilation. "))


;; All packages currently loaded
;; Functions to manipulate packages and modules



;; Functions to get information from 'raw' (read) syntax trees.
(declaim (ftype (function (silica-package list) module) build-module))
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

       ;; Ensure that each exported field is actually exported from the module.
       ;; if a module re-exports an imported value, then appropriate definitions
       ;; are inserted in at the end of the buffer 
       ;; (note: also update the signature!)
       (ensure-exports (module env ctx)
         (iter 
           (for field in (al:lookup :export-list module))
           (with type = (al:lookup :type module))
           (with val = (al:lookup :typed-ast module))

                (unless (and (get-field type field) (get-field val field))
                  ;; The field is not defined in this module, so we must check
                  ;; if this value was imported. If if yes, then modify the type
                  ;; & typed ast to match. If no, then raise an error.  
                  (if (and (env:lookup field env) (ctx:lookup field ctx))
                      (progn
                        (collect (mk-decl field (env:lookup field env))
                          into pass-types)
                        (collect (mk-def field (ctx:lookup field ctx))
                          into pass-values))
                      (error (format nil "can't find export: ~A" field))))

                (finally
                 (return
                   (->> module
                        (al:insert :type (update-sig type pass-types))
                        (al:insert :typed-ast (update-sct val pass-values)))))))

       (update-sig (signature new-fields)
         (mk-sig
          (append
           (entries signature)
           (li:map (lambda (dec) (mk-entry (var dec) dec)) new-fields))))

       (update-sct (structure new-fields)
         (mk-struct
          (append
           (entries structure)
           (li:map (lambda (def) (mk-entry (var def) def)) new-fields))))

       (reify-module (module env)
         (al:insert 
          :code
          (reify (al:lookup :typed-ast module) env)
           module)))

    (let* (;; TODO: also account for modules at the level of the package!
           (modules (get-available-modules package nil))

           ;; Generate typechecking environment from imported modules + module
           ;; definition.
           (env (gen-env modules module-raw))

           (ctx (gen-ctx modules module-raw))
           ;; TODO add parser fixity generation
           )

      (-> module-raw
          (parse-module)
          (type-module env)
          (ensure-exports env ctx)
          ((lambda (m)
            m))
          (reify-module ctx)))))

(declaim (ftype (function (hash-table list) env) gen-env))
(defun gen-env (available-modules module-raw)
  (env:make-from
   (process-import-declaration
    available-modules
    module-raw
    (lambda (type val)
      (ecase type
        (:module (module-export-types val))
        (:atom (atom-export-types val))))
    (lambda (module) (signature module))
    (lambda (atom) (al:lookup :type atom)))))

(declaim (ftype (function (hash-table list) ctx:context) gen-ctx))
(defun gen-ctx (available-modules module-raw)
  (ctx:make-from 
   (process-import-declaration
    available-modules
    module-raw
    (lambda (type val)
      (ecase type
        (:module (module-export-values val))
        (:atom (li:map
                (lambda (entry)
                  (list (var entry)
                        (al:make 
                         (:code . `(gethash (quote ,(var entry))
                                            ,(al:lookup :code val)))
                         (:type . (ann (binder entry))))))
                (entries (al:lookup :type val))))))
    (lambda (module) (lisp-val module))
    (lambda (atom) (al:lookup :code atom)))))

(declaim (ftype (function (hash-table list
                                      (function (symbol t) t)
                                      (function (t) t)
                                      (function (t) t))
                          hash-table)
                process-import-declaration))
(defun process-import-declaration
    (available-modules module-raw contents-getter
     process-module-val process-atom-val)
  "Generate the type-checking environment for a particular module."
  (let* ((imports (al:lookup :import-list module-raw))

         (merge-func
           (lambda (table-1 table-2)
             (if table-2
                 (ht:merge
                  table-1 table-2
                  :by (alexandria:curry #'list :ambiguous-name))
                 table-1)))

         ;; take an import-table (see get-imports, below) and generate a
         ;; hashtable that is an export list.
         ;; TODO: in the future, there may be name collisions. Make sure we
         ;; update this code to match & generate an error.
         (process
           (lambda (import-table)
             (iter (for (type name val) in import-table)
               (with out = (ht:empty))

               (ecase type
                 (:atom (setf (gethash name out) (funcall process-atom-val val)))
                 (:module (setf (gethash name out) (funcall process-module-val val))))
               
               (finally (return out)))))

         ;; generate list of imported types
         (get-imports
           (lambda (import-decl)
             (let ((module
                       (gethash (car import-decl) available-modules)))


               (iter (for elt in (cdr import-decl))
                 ;; the import table contains
                 ;; elements which are values (:atom name val)
                 ;; elements which are modules (but may be values) (:module name val)
                 (with import-table = (list (list :module (car import-decl) module)))

                 ;; calculate the new value of the import table by 'stepping' on
                 ;; element along the import declaration
                 (setf
                  import-table
                  (iter (for (type nm val) in import-table)
                    (for types = (funcall contents-getter type val))
                    (declare (ignorable nm))

                    (etypecase elt
                      (list
                       (collect 
                           (iter (for import-name in elt)
                             (if (pkg-wildcard-sym? import-name)
                                 (return (li:map (curry #'cons :atom) types))
                                 (collect (cons :atom (assoc import-name types)))))
                       into new-table))
                      (symbol
                       (if (pkg-wildcard-sym? elt)
                           (collect (li:map (curry #'cons :atom) types)
                             into new-table)
                           (collect (list (cons :atom (assoc elt types)))
                             into new-table))))

                    (finally
                     (return (li:join new-table)))))

                 (finally
                  (return (funcall process import-table))))))))

    ;; for now, assume imports are a list of symbols which designate packages
    (iter (for import-decl in imports)

      ;; TODO: expand this so it takes into account whether modules have
      ;; specific fields mentioned in the import-decl 
      (when (not (gethash (car import-decl) available-modules))
        (error (format nil "Couldn't find module of name ~A" (car import-decl))))
      
      ;; Accumulate available symbols as a hash-table
      (accumulate 
       (funcall get-imports import-decl)
       by merge-func
       into imported-values
       initial-value (ht:empty))

      (finally
       (return imported-values)))))

(defun module-export-types (module)
  "Get all exported types from a module as a list ((name type) (name type) ...)
we assume no collisions (this should be checked by the exporting module)"

  (iter (for symbol in (exports module))
    (let ((tipe (get-field (signature module) symbol)))
      (typecase tipe
        (kind
         (collect
             (list symbol
                   (al:make
                    (:type . (cons tipe
                                 (get-field (internal-struct module) symbol)))
                    (:val . (get-field (internal-struct module) symbol))))))
         (t (collect
                (list symbol
                      (al:make
                       (:type . tipe)
                       (:val . (get-field (internal-struct module) symbol))))))))))

(defun atom-export-types (atom)
  "Get all exported types from an atom (signature) as a list ((name type) (name
  type) ...). Any non-sealed types are exported as (name (kind . type)), while
  sealed types are exported as (name . kind)."
  (iter (for entry in (entries (al:lookup :type atom)))
    (let ((type (get-field (al:lookup :type atom) (var entry))))
      (typecase type
        (kind
         (collect
             (list (var entry)
                   (al:make 
                    (:type . (cons type (get-field (al:lookup :val atom) (var entry))))
                    (:val . (get-field (al:lookup :val atom) (var entry)))))))
        (t (collect
               (list (var entry)
                     (al:make
                      (:type . type)
                      (:val . (get-field (al:lookup :val atom) (var entry)))))))))))


(defun module-export-values (module)
  "get all export values (for code-generation) from a module as a list
  ((name value) (name value) ...) we assume no collisions (this should be when
  the module is generated)" 
  (iter (for symbol in (exports module))
    (collect
        (list symbol
              (al:make
               (:code . `(gethash (quote ,symbol) (lisp-val ,module)))
               (:type . (get-field (signature module) symbol)))))))


(declaim (ftype (function (t) silica-package) get-package))
(defun get-package (name)
  (gethash name *packages*))

(declaim (ftype (function (silica-package) hash-table) get-exports))
(defun get-exports (package)
  "Get a list of exported modules from a package"
  (iter (for name in (exported-modules package))
    (with exports = (ht:empty))
    (setf (gethash name exports)
          (ht:lookup name (modules package)))

    (finally (return exports))))

(declaim (ftype (function (silica-package list) hash-table) get-available-modules))
(defun get-available-modules (package path)
  "Given a package, and a path to a module inside that package (not yet built),
get a list of all modules available for import inside that module."
  (declare (ignore path))
  (flet ((merge-exports (t1 t2)
          (ht:merge t1 t2
                    :by (alexandria:curry #'list :ambiguous-name))))
    (merge-exports
     (iter (for pkg-name in (dependencies package))
       (accumulate
        (get-exports (get-package pkg-name))
        by #'merge-exports
        :initial-value (ht:empty)))
     (iter (for (name module) in-hashtable (modules package))
       (accumulate
        (ht:make (name . module))
        by #'merge-exports
        :initial-value (ht:empty))))))

(declaim (ftype (function (silica-package) hash-table) get-externally-available-modules))
(defun get-externally-available-modules (package)
  "Given a package, get a list of modules which should be available for import
in all it's modules."
  (iter (for pkg-name in (dependencies package))
    (accumulate
     (get-exports (get-package pkg-name))
     by (lambda (t1 t2)
          (ht:merge t1 t2
                    :by (alexandria:curry #'list :ambiguous-name)))
     :initial-value (ht:empty))))
