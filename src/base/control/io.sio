(module io
  (export IO bind >>= seq pure))

(World ⮜ τ)
(World ≜ native ⟦real-world⟧)

(IO ⮜ τ → τ)
(IO α ≜ World → α)

(pure ⮜ ∀ α (α → IO α))
(pure ≜ Λ α (λ (x world) x))

((>>=) ⮜ ∀ (α β) (IO α → (α → β) → IO β))
((>>=) ≜ Λ (α β)
   (λ (m f) (λ (world) (f (m world)))))

(bind ⮜ ∀ (α β) (IO α → (α → IO β) → IO β))
(bind ≜ Λ (α β)
   (λ (m f) (λ (world) (f (m world) world))))

⍝ ((>>) ;le ∀ (α β) (IO α → IO β → IO β))
⍝ ((>>) ≜ Λ (α β)
⍝   (λ (m f) (λ (world) (f (m world)))))

((seq) ⮜ ∀ (α β) (IO α → IO β → IO β))
((seq) ≜ Λ (α β)
   (λ (m1 m2) (λ world ((λ ((x ⮜ α)) (m2 world)) (m1 world)))))