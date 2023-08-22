(module string
  (export Text ⋅))

(Text ◂ τ)
(Text ≜ native ⟦(cl:simple-array cl:character)⟧)

((⋅) ◂ Text → Text → Text)
((x ⋅ y) ≜ lisp Text (x y) ⟦(cl:concatenate 'string sym::|x| sym::|y|)⟧)
