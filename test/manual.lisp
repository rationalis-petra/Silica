;; Manual testing area

(NAMED-READTABLES:IN-READTABLE :MODERN) 

(CL:IN-PACKAGE :OPAL-USER) 

(module main
  ;; number type
  (ℕ ◂ κ)
  (ℕ ≜ ∀ α (α → (α → α) → α))

  (zero ◂ ℕ)
  (zero ≜ Λ α (λ (base next) base))

  (succ ◂ ℕ → ℕ)
  (succ n ≜ Λ α (λ (base next) (next (n α base next))))

  ;; List type
  (List ◂ κ → κ)
  (List ≜ λ A (∀ β (β → (A → β → β) → β)))

  (nil ◂ ∀ α (List α))
  (nil ≜ Λ (α β) (λ (base rec) base))

  (cons ◂ ∀ α (α → List α → List α))
  (cons ≜ Λ α (λ (val list) (Λ β (λ (base rec) (rec val (list β base rec))))))

  ;; TODO: fix the typechecker so I work!
  (map ◂ ∀ (α β) ((α → β) → List α → List β))
  (map ≜ Λ (α β) (λ (f l) (l (List β)
                             (nil β)
                             (λ ((val ◂ α) (rest ◂ List β))
                                (cons β (f val) rest)))))

  ;; (Monad ◂ (κ → κ) → κ) 
  ;; (Monad M ≜
  ;;   Σ (pure ◂ ∀ α (α → M α))
  ;;     (bind ◂ ∀ (α β) ((α → β) → M α → M β)))

  ;; (pure ◂ ∀ ((M ◂ κ → κ) α) (Monad M → α → M α))
  ;; (pure ≜ Λ (M α) (λ (monad val) ((π pure monad) val)))

  ((*) ◂ κ → κ → κ)
  ((*) ≜ λ (α β) (Σ (fst ◂ α) (snd ◂ β)))

  (fst ◂ ∀ (α β) ((α * β) → α))
  (fst ≜ Λ (α β) (λ pair (π fst pair)))

  ;; (snd : ∀ (α β) ((α * β) → β)


  ;; Lisp Bridge
  
  (ClList ◂ κ)
  (ClList ≜ native CL:INTEGER)

  (to-clist ◂ ∀ α (List α → ClList))
  (to-clist ≜ Λ α (λ lst (lisp ClList (lst)
                               (OPAL/UTIL:APP-CURRY lst CL:NIL
                                                    (CL:LAMBDA (x)
                                                      (CL:LAMBDA (y)
                                                        (CL:CONS x y)))))))

  (ℤ ◂ κ)
  (ℤ ≜ native CL:INTEGER)

  ;; TODO: fix the typechecker so I work!
  ;; (pr ◂ ℤ * ℤ)
  ;; (pr ≜ σ (x ≜ to-int zero) (y ≜ to-int (succ zero)))

  (to-int ◂ ℕ → ℤ)
  (to-int n ≜ (lisp ℤ (n) (OPAL/UTIL:APP-CURRY n 0 (CL:LAMBDA (x) (CL:+ x 1)))))



  ;; Experimentation

  ;; (nat-list ◂ ∀ α (List (ℕ α)))
  ;; (nat-list ≜ Λ α ())
  (one ≜ succ zero)
  (two ≜ succ (succ zero))

  ;; TODO: map (to-int) (cons ℤ one (cons ℤ two (nil ℤ )))
  (list-val ≜ to-clist ℤ (map ℕ ℤ to-int (cons ℕ one (cons ℕ two (nil ℕ)))))
  )


(NAMED-READTABLES:IN-READTABLE ()) 
 
