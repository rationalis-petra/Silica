(in-package :opal.tests)

(define-test parsing
    (define-test infixify
      (is equal
          '(+ 1 2)
          (infixify '(1 + 2)))

      (is equal
          '(+ 1 2)
          (infixify '((1) + 2)))

      ;; Special cases of non-infix math characters
      (is equal
          '(1 ∀ 2)
          (infixify '(1 ∀ 2)))
      
      (is equal
          '(1 ∃ 2)
          (infixify '(1 ∃ 2)))

      (is equal
          '(sym::|lisp| (+ 1 2) nil (3 - 4))
          (infixify '(sym::|lisp| ((1) + 2) nil (3 - 4)))))

  (define-test to-def)
  (define-test to-ast)
  (define-test module))
