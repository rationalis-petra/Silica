(module bool
  (export Bool true false))


(Bool ⮜ τ)
(Bool ≜ (native ⟦cl:boolean⟧))

(true ⮜ Bool)
(true ≜ (lisp Bool () ⟦cl:t⟧))

(false ⮜ Bool)
(false ≜ (lisp Bool () ⟦cl:nil⟧))

((∧) ⮜ Bool → Bool → Bool)
((x ∧ y) ≜ (lisp Bool (x y) ⟦cl:if sym::|x| sym::|y| sym::|x|⟧))

((∨) ⮜ Bool → Bool → Bool)
((x ∨ y) ≜ (lisp Bool (x y) ⟦cl:if sym::|x| sym::|x| sym::|y|⟧))

(not ⮜ Bool → Bool)
(not x ≜ (lisp Bool (x) ⟦cl:not sym::|x|⟧))