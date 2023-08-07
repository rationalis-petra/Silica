;; Manual testing area

(NAMED-READTABLES:IN-READTABLE :MODERN) 

(CL:IN-PACKAGE :OPAL-USER) 

(module main
  (Unit ◂ τ)
  (Unit ≜ native CL:INTEGER)

  (unit ◂ Unit)
  (unit ≜ (lisp Unit () 0))

  (IO ◂ τ → τ)
  (IO ≜ ∀ α (Unit → α))

  ;; (pure ◂ ∀ α (α → IO α))
  ;; (pure ≜ Λ α. λ x. (λ f. ) )

  (print ◂ String → IO Unit)
  (print string ≜ λ v (lisp (Unit → Unit) ()
                            (CL:LAMBDA ()
                              (CL:PRINC string)
                              (OPAL:MK-VAL 0))))
  ;; (print x ≜ λ f. (λ ) (lisp (Unit → IO Unit) (x) (CL:LAMBDA (v) CL:PRINC x)))

  (id ◂ ∀ α (α → α))
  (id ≜ Λ α (λ x x))

  (ℤ ◂ τ)
  (ℤ ≜ native CL:INTEGER)

  ((+) ◂ ℤ → ℤ → ℤ)
  ((x + y) ≜ (lisp ℤ (x y) (CL:+ x y)))

  ((-) ◂ ℤ → ℤ → ℤ)
  ((x - y) ≜ (lisp ℤ (x y) (CL:- x y)))

  (x ≜ (2 - 3) + id ℤ 3))


(NAMED-READTABLES:IN-READTABLE ()) 
