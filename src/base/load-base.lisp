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
      ;; (format t "build-output: ~%~A~%" build-output)
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
               :exported-modules (list (sym "int")))))
    (load-base-module (sym "int") "numerics/integer" base)

    (setf (gethash (sym "base") *packages*) base)))





