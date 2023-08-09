;; Manual testing area

(NAMED-READTABLES:IN-READTABLE :MODERN) 

(CL:IN-PACKAGE :OPAL-USER) 

(module main
  ;; number type
  (ℕ ◂ τ → τ)
  (ℕ ≜ ∀ α (α → (α → α) → α))

  (zero ◂ ℕ)
  (zero ≜ Λ α (λ (base next) base))

  (succ ◂ ℕ → ℕ)
  (succ n ≜ Λ α (λ (base next) (next (n α base next))))

  ;; List type
  (List ◂ τ → τ → τ)
  (List ≜ ∀ (α β) (β → (α → β → β) → β))

  (nil ◂ List)
  (nil ≜ Λ (α β) (λ (base rec) base))

  (cons ◂ ∀ α (α → List α → List α))
  (cons ≜ Λ α (λ (val list) (Λ β (λ (base rec) (rec val (list β base rec))))))

  ;; TODO: fix the typechecker so I work!
  ;; (map ◂ ∀ (α β) ((α → β) → List α → List β))
  ;; (map ≜ Λ (α β) (λ (f l) (Λ γ (l (List β γ) nil (λ (val rest) (cons β (f val) rest))))))

  ((*) ◂ τ → τ → τ)
  ((*) ≜ ∀ (α β) (Σ (fst ◂ α) (snd ◂ β)))

  (ClList ◂ τ)
  (ClList ≜ native CL:INTEGER)

  (to-clist ◂ ∀ α (List α → ClList))
  (to-clist ≜ Λ α (λ lst (lisp ClList (lst)
                               (OPAL/UTIL:APP-CURRY lst CL:NIL
                                                    (CL:LAMBDA (x)
                                                      (CL:LAMBDA (y)
                                                        (CL:CONS x y)))))))

  (ℤ ◂ τ)
  (ℤ ≜ native CL:INTEGER)

  ;; TODO: fix the typechecker so I work!
  ;; (pr ◂ ℤ * ℤ)
  ;; (pr ≜ σ (x ≜ to-int zero) (y ≜ to-int (succ zero)))

  (to-int ◂ ℕ → ℤ)
  (to-int n ≜ (lisp ℤ (n) (OPAL/UTIL:APP-CURRY n 0 (CL:LAMBDA (x) (CL:+ x 1)))))

  ;; (nat-list ◂ ∀ α (List (ℕ α)))
  ;; (nat-list ≜ Λ α ())
  (one ≜  (succ zero))
  (two ≜ to-int (succ (succ zero)))

  ;; TODO: map (to-int) (cons ℤ one (cons ℤ two (nil ℤ )))
  (list-val ≜ to-clist ℤ (cons ℤ (to-int (succ zero)) (cons ℤ two (nil ℤ)))))


(NAMED-READTABLES:IN-READTABLE ()) 
 
