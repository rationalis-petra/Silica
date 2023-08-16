
(module array
  (export
   Array
   acons
   anil
   ))

(Array ◂ τ → τ)
(Array ≜ λ α. native (α) (OPAL/TYPES:ARRAY α))


(instance ∀ α. Monoid (Array α) ≜ σ
  (empty ≜ (lisp (Array α) (mk-array)))

  (◇ ≜ Λ α. λ x y.
    (lisp (Array α) (x y)
      [|(OPAL/ARRAY:APPEND x y)|])))

(acons ◂ ∀ α. α → Array α → Array α)
(acons ⟨α⟩ v arr ≜ (lisp (Array α) (v arr)
  [|(OPAL/ARRAY:ACONS v arr)|]))

(anil ◂ ∀ α. Array α)
(anil ⟨α⟩ ≜ (lisp (Array α) () [|(OPPAL/ARRAY:ANIL)|]))