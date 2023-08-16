(module integer
 (export
  ℤ (+) (-) (×) (÷)))

(ℤ ◂ τ)
(ℤ ≜ (native CL:INTEGER))

((+) ◂ ℤ → ℤ → ℤ)
(x + y ≜ (lisp ℤ (x y) (Cl:+ x y)))

((-) ◂ ℤ → ℤ → ℤ)
(x - y ≜ (lisp ℤ (x y) (Cl:- x y)))

((×) ◂ ℤ → ℤ → ℤ)
(x × y ≜ (lisp ℤ (x y) (Cl:* x y)))

((÷) ◂ ℤ → ℤ → ℤ)
(x ÷ y ≜ (lisp ℤ (x y) (Cl:FLOOR x y)))

