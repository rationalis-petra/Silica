(in-package :opal)


(defun base-path (path)
  (asdf:system-relative-pathname
   :opal
   (concatenate 'string "src/base/" path ".opal")))

;; laod the base library
(defun load-base-module (name path package)
  (with-open-file (file (base-path path))
    (let* ((module-raw (parse-file file))
           (build-output (build-module package module-raw)))
      (setf (gethash name (modules package))
            (make-instance
             'module
             :name name
             :source path
             ;; TODO: make this a function!
             :dependencies (al:lookup :import-list module-raw)
             :signature (al:lookup :type build-output) 
             :internal-struct (al:lookup :typed-ast build-output)
             :lisp-val (eval (al:lookup :code build-output))
             :module-package package
             :exports (al:lookup :export-list build-output))))))

(defun load-base ()
  "A hacky solution (while quartz is under development) for loading in the base
package for use in other projects/packages."
  (let ((base (make-instance
               'opal-package
               :name (sym "base")
               :exported-modules
               (list
                (sym "unit")
                (sym "bool")
                (sym "int")
                (sym "float")

                (sym "text")

                (sym "io")

                (sym "console")
                ))))
    ;; TODO: add a num module

    (load-base-module (sym "bool") "num/bool" base)
    (load-base-module (sym "int") "num/int" base)
    (load-base-module (sym "float") "num/float" base)
    (load-base-module (sym "unit") "num/unit" base)

    (load-base-module (sym "text") "data/text" base)

    (load-base-module (sym "io") "control/io" base)

    (load-base-module (sym "console") "sys/console" base)

    (setf (gethash (sym "base") *packages*) base)))





