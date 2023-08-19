(in-package :opal.tests)

(define-test parsing
  :parent all

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
  (define-test to-ast
      (is α= (mk-var 'x) (to-ast 'x))

    (is α=
        (mk-∀ 'x (mk-var 'x))
        (to-ast '(sym::|∀| x x)))

    (is α=
        (mk-sig (entries
                 (mk-decl 'x (mk-var 'a))
                 (mk-decl 'y (mk-var 'b))))
        (to-ast '(sym::|Σ| (sym::◂ x a) (sym::◂ y b)))))

  (define-test module))
