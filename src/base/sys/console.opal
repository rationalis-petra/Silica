(module console
  (import 
    io.(…)
    unit.(…) 
    text.(…))
  (export print readLine))

(print ⮜ Text → IO Unit)
(print s ≜ lisp (IO Unit) (s)
 ⟦(cl:lambda (x) 
    (cl:declare (cl:ignore x))
    (cl:print sym::|s|)
    t)⟧)

(readLine ⮜ IO Text)
(readLine ≜ lisp (IO Text) ()
 ⟦(cl:lambda (x)
    (cl:declare (cl:ignore x))
    (cl:read-line))⟧)