(module console
  (import io unit string)
  (export print))

(print ◂ String → IO Unit)
(print s ≜ lisp (IO Unit) (s)
 ⟦(cl:lambda (x) 
    (cl:declare (cl:ignore x))
    (cl:print sym::|s|)
    t)⟧)