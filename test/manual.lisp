;; Manual testing area

(named-readtables:in-readtable :modern) 

(CL:IN-PACKAGE :OPAL-USER) 

(module main
  (id : (∀ α (→ α α)))
  (id ≜ (Λ α (λ x x))))

;; (module main
;;   (pure x   ≜ (lisp :auto (x) (CL:LAMBDA () x)))
;;   (>> m1 m2 ≜ (lisp :auto (x) (CL:LAMBDA () (CL:FUNCALL m1) (CL:FUNCALL m2))))
;;   (>>= m f  ≜ (lisp :auto (x) (CL:LAMBDA () (CL:FUNCALL m (CL:FUNCALL f)))))

;;   (print-ln string ≜ (lisp :auto (string) (CL:LAMBDA () (CL:PRINT string))))

;;   (add x y ≜ (lisp ℤ (x y) (CL:+ x y)))
;;   (sub x y ≜ (lisp ℤ (x y) (CL:- x y)))

;;   ;; (main ≜ do
;;   ;;   (line ← read-ln)
;;   ;;   (print-ln ("hello, " ⋅ line ⋅ "!"))
;;   ;;   (pure 2))

;;   (main ≜ (>> (print-ln "hello-world!") (pure 2))))


(NAMED-READTABLES:IN-READTABLE CL:NIL) 

;; (opal:run-main)

;; IO Monad Representation
;; pure x → (lambda () x)
;; m >> m' → (lambda () (funcall m) (funcall m'))
;; m >>= f → (lambda () (funcall f (funcall m)))
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
