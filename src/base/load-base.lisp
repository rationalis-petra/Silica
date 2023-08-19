(in-package :opal)


;; laod the base library


(defun load-base ()
  "A hacky solution (while quartz is under development) for loading in the base
package for use in other projects/packages."
  (let ((base (make-instance
               'opal-package
               :name "base"
               :exported-modules (list "int"))))
    (load-base-module "int" "numerics/integer" base)

    (setf (gethash "base" *packages*) base)))

(defun base-path (path)
  (asdf:system-relative-pathname
   :opal
   (concatenate 'string "src/base/" path ".opal")))

(defun load-base-module (name path package)
  (with-open-file (file (base-path path))
    (let* ((module-raw (parse-file file))
           (module-struct (build-module package module-raw)))
      (setf (gethash name (modules package))
            (make-instance
             'module
             :name name
             :source path
             :dependencies () ; (get-dependencies module-raw)
             :internal-struct module-struct
             :module-package package
             :exports (get-exports module-raw))))))


(defun test-load ()
  (with-open-file (file (base-path "numerics/integer"))
    (let* ((module-raw (parse-file file)))
      module-raw)))
