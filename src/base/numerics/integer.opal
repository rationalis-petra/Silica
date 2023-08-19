(module integer
 (export
  ℤ (+) (-) (×) (÷)))

(ℤ ◂ τ)
(ℤ ≜ (native CL:INTEGER))

((+) ◂ ℤ → ℤ → ℤ)
((x + y) ≜ (lisp ℤ (x y) ⟦(cl:+ sym::|x| sym::|y|)⟧))

((-) ◂ ℤ → ℤ → ℤ)
((x - y) ≜ (lisp ℤ (x y) ⟦(cl:- sym::|x| sym::|y|)⟧))

((×) ◂ ℤ → ℤ → ℤ)
((x × y) ≜ (lisp ℤ (x y) ⟦(cl:* sym::|x| sym::|y|)⟧))

((÷) ◂ ℤ → ℤ → ℤ)
((x ÷ y) ≜ (lisp ℤ (x y) ⟦(cl:floor sym::|x| sym::|y|)⟧))

