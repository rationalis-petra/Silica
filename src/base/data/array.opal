
(module array
  (export
   Array
   acons
   anil
   ))

(Array ◂ τ → τ)
(Array ≜ λ α. native (α) ⟦opal/array:type sym::|α|⟧)


⍝ (using arr-monoid)
(arr-monoid ◂ ∀ α. Monoid (Array α))
(arr-monoid ≜ σ
  (empty ≜ lisp (Array α) () ⟦opal/array:+empty+⟧)

  (◇ ≜ Λ α. λ x y.
    lisp (Array α) (x y)
      ⟦(opal/array:append sym::|x| sym::|y|)⟧))

(acons ◂ ∀ α. α → Array α → Array α)
(acons ⟨α⟩ v arr ≜ lisp (Array α) (v arr)
  ⟦(OPAL/ARRAY:ACONS v arr)⟧)

(anil ◂ ∀ α. Array α)
(anil ⟨α⟩ ≜ lisp (Array α) () ⟦(OPPAL/ARRAY:ANIL)⟧)