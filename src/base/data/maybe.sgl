(module maybe
  (import 
    abs.(functor.(*)
         applicative.(*)
         monad.(*))))

(Maybe : ◂ τ → τ)
(Maybe ≜ Φ
  (some : ∀ α. α → Maybe α)
  (none : ∀ α. Maybe α))

⍝ (implicit maybe-functor)
(maybe-functor ◂ Functor Maybe)
(maybe-functor ≜ σ
  (map f m ≜ φ m
    (some x → some ⧺ f x)
    (none → none)))

⍝ (implicit maybe-applicative)
(maybe-applicative ◂ Applicative Maybe)
(maybe-applicative ≜ σ
  (include maybe-functor)
  (l ◆ r ≜ φ l
    (some f → map f r)
    (none → none))

  (pure v ≜ some v))


⍝ (implicit maybe-monad)
(maybe-monad ◂ Monad Maybe)
(maybe-monad ≜ σ
  (include maybe-applicative)
  (m >>= f ≜ φ m
    (some v → f v)
    (none → none)))