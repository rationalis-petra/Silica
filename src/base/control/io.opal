(module io
  (export IO >>= pure))

(World ◂ τ)
(World ≜ native ⟦real-world⟧)

(IO ◂ τ → τ)
(IO α ≜ World → α)

(pure ◂ ∀ α (α → IO α))
(pure ≜ Λ α (λ (x world) x))

((>>=) ◂ ∀ (α β) (IO α → (α → β) → IO β))
((>>=) ≜ Λ (α β)
   (λ (m f) (λ (world) (f (m world)))))