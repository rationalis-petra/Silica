;; Manual testing area

(NAMED-READTABLES:IN-READTABLE :MODERN) 

(CL:IN-PACKAGE :OPAL-USER) 

(module main
  (import integer)
  ;; unit type
  (Unit ◂ τ)
  (Unit ≜ ∀ α (α → α))

  (unit ◂ Unit)
  (unit ≜ Λ α (λ x x))
  
  ;; number type
  (ℕ ◂ τ)
  (ℕ ≜ ∀ α (α → (α → α) → α))

  (zero ◂ ℕ)
  (zero ≜ Λ α (λ (base next) base))

  (succ ◂ ℕ → ℕ)
  (succ n ≜ Λ α (λ (base next) (next (n α base next))))

  ;; Bool Type
  (Bool ◂ τ)
  (Bool ≜ ∀ α (α → α → α))

  (true ◂ Bool)
  (true ≜ Λ α (λ (x y) x))

  (false ◂ Bool)
  (false ≜ Λ α (λ (x y) y))

  ;; List type
  (List ◂ τ → τ)
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

  (∃ ◂ (τ → τ) → τ)
  (∃ F ≜ Σ (A ◂ τ) (v ◂ F A))

  ;(Monad ◂ ∃ Monad)

  (Monad ◂ τ) 
  (Monad ≜ Σ
    (M ◂ τ → τ)
    (pure ◂ ∀ α (α → M α))
    (bind ◂ ∀ (α β) ((α → β) → M α → M β)))

  ;; (pure ◂ ∀ ((M ◂ τ → τ) α) (Monad M → α → M α))
  ;; (pure ≜ Λ (M α) (λ (monad val) ((π pure monad) val)))

  ((*) ◂ τ → τ → τ)
  ((*) ≜ λ (α β) (Σ (fst ◂ α) (snd ◂ β)))

  (fst ◂ ∀ (α β) ((α * β) → α))
  (fst ≜ Λ (α β) (λ pair (π fst pair)))

  ;; (snd : ∀ (α β) ((α * β) → β)


  ;; Lisp Bridge
  
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

  (to-int ◂ ℕ → ℤ)
  (to-int n ≜ (lisp ℤ (n) (OPAL/UTIL:APP-CURRY n 0 (CL:LAMBDA (x) (CL:+ x 1)))))

  (clBool ◂ τ)
  (clBool ≜ native CL:BOOLEAN)

  (to-bool ◂ Bool → clBool)
  (to-bool b ≜ b clBool (lisp clBool () (CL:T)) (lisp clBool () (CL:NIL)))

  ;; Experimentation

  ;; (nat-list ◂ ∀ α (List (ℕ α)))
  ;; (nat-list ≜ Λ α ())
  (one ≜ succ zero)
  (two ≜ succ (succ zero))

  ;; TODO: map (to-int) (cons ℤ one (cons ℤ two (nil ℤ )))
  (my-list ≜ (cons ℕ one (cons ℕ two (nil ℕ))))

  (out ≜ to-clist ℤ (map ℕ ℤ to-int my-list))


  ;; 
  (IO ◂ τ → τ)
  (IO ≜ λ A. unit → A)

  
)


(NAMED-READTABLES:IN-READTABLE ()) 
 
