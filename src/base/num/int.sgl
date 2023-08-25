(module int
 (export ℤ + - × ÷ = < > ≥  ≤))

(ℤ ⮜ τ)
(ℤ ≜ native ⟦cl:integer⟧)

((+) ⮜ ℤ → ℤ → ℤ)
((x + y) ≜ (lisp ℤ (x y) ⟦(cl:+ sym::|x| sym::|y|)⟧))

((-) ⮜ ℤ → ℤ → ℤ)
((x - y) ≜ (lisp ℤ (x y) ⟦(cl:- sym::|x| sym::|y|)⟧))

((×) ⮜ ℤ → ℤ → ℤ)
((x × y) ≜ (lisp ℤ (x y) ⟦(cl:* sym::|x| sym::|y|)⟧))

((÷) ⮜ ℤ → ℤ → ℤ)
((x ÷ y) ≜ (lisp ℤ (x y) ⟦(cl:floor sym::|x| sym::|y|)⟧))



((=) ⮜ ℤ → ℤ → Bool)
((x = y) ≜ (lisp Bool (x y) ⟦(cl:= sym::|x| sym::|y|)⟧))

((<) ⮜ ℤ → ℤ → Bool)
((x < y) ≜ (lisp Bool (x y) ⟦(cl:< sym::|x| sym::|y|)⟧))

((>) ⮜ ℤ → ℤ → Bool)
((x > y) ≜ (lisp Bool (x y) ⟦(cl:> sym::|x| sym::|y|)⟧))

((≥) ⮜ ℤ → ℤ → Bool)
((x ≥ y) ≜ (lisp Bool (x y) ⟦(cl:>= sym::|x| sym::|y|)⟧))

((≤) ⮜ ℤ → ℤ → Bool)
((x ≤ y) ≜ (lisp Bool (x y) ⟦(cl:<= sym::|x| sym::|y|)⟧))