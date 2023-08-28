(in-package :silica)


;; laod the base library

(defun test-path (path)
  (asdf:system-relative-pathname
   :silica
   (concatenate 'string "test/manual/" path ".sio")))

(defun load-test-module (name path package)
  (with-open-file (file (test-path path))
    (let* ((module-raw (parse-file file))
           (build-output (build-module package module-raw)))
      (setf (gethash name (modules package))
            (make-instance
             'module
             :name name
             :source (test-path path)
             :signature (al:lookup :type build-output) 
             :internal-struct (al:lookup :typed-ast build-output)
             :lisp-val (eval (al:lookup :code build-output))
             :module-package package
             :exports (al:lookup :export-list build-output))))))


(defun load-test ()
  "A hacky solution (while quartz is under development) for loading in the base
package for use in other projects/packages."
  (let ((test (make-instance
               'silica-package
               :name (sym "test")
               :dependencies (list (sym "base"))
               :exported-modules (list (sym "test")))))
    (load-test-module (sym "test") "test" test)

    (setf (gethash (sym "test") *packages*) test)))

 
;;  (Expr ◂ τ → τ)
;;  (Expr ≜ Φ
;;    (lit-bool ≜ Bool → Expr Bool)
;;    (lit-int ≜ ℤ → Expr ℤ)
;;    (lift ≜ ∀ α β. (α → β) → Expr (α → β))
;;    (app ≜ ∀ α β. Expr (α → β) → Expr α → Expr β))
;;  
;;  (eval ◂ ∀ α. Expr α → α)
;;  (eval ⦅α⦆ expr ≜ μ expr
;;    (lit-bool b → b)
;;    (lit-int n → n)
;;    (lift ⦅α β⦆ f → f)
;;    (app ⦅α β⦆ fnc val → (eval (α → β) fnc) (eval α val)))
;;  
;;  (result ≜ (eval ℤ (lift (+) (lit-int 2) (lit-int 5))))
