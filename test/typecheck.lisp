(in-package :opal.tests)

(defparameter int-type (make-instance
                        'opal::native-type
                        :native-type 'integer))


(define-test typecheck
  (define-test type-check
    ;; w : int
    (true (opal:check (mk-val 2) int-type nil))

    ;; (λ x. 2) : int → int
    (true (opal:check (mk-λ 'x (mk-val 2))
                      (mk-arr int-type int-type)
                      nil))

    ;; (Λ a. λ x. x) : ∀ a. a → a
    (true (opal:check (mk-abs 'a (mk-λ 'x (mk-var 'x)))
                      (mk-∀ 'a (mk-arr (mk-var 'a) (mk-var 'a)))
                      nil))

    ;; (σ (x ≜ 2) (y ≜ 3)) : (Σ (x : int) (y : int))
    (true (opal:check (mk-struct (list (mk-def 'x (mk-val 2))
                                       (mk-def 'y (mk-val 2))))
                      (mk-sig (list (mk-decl 'x int-type)
                                    (mk-decl 'y int-type)))
                      nil)))

  (define-test type-infer))
