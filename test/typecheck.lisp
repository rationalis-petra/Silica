(in-package :opal.tests)

;; https://gist.github.com/zmactep/c5e167c86fb8d80dcd5532792371863f
;; https://arxiv.org/abs/1803.02473
;; https://www.reddit.com/r/haskell/comments/3tajp9/an_alternative_to_church_scott_and_perigot/

(defparameter int-type (make-instance
                        'opal::native-type
                        :native-type 'integer))

(defparameter τ (mk-kind))

(define-test typing
  (define-test type-check
    ;; int : τ
    (is α= int-type
        (opal:check int-type τ +empty-env+))

    ;; int → int : τ
    (is α= (mk-arr int-type int-type)
        (opal:check (mk-arr int-type int-type) τ +empty-env+))

    ;; (bot) ∀ a. a ◂ τ
    (is α= (mk-∀ 'a (mk-tvar 'a))
        (opal:check (mk-∀ 'a (mk-tvar 'a)) τ +empty-env+))

    ;; (Id) λ α. α ◂ τ → τ
    (is α=
        (mk-tλ 'a τ (mk-tvar 'a))
        (opal:check (mk-λ 'a (mk-var 'a)) (mk-karr τ τ) +empty-env+))

    ;; (List) λ A. ∀ β (β → (A → β → β) → β) ◂ τ → τ
    (is α=
        (mk-tλ 'A τ (mk-∀ 'β τ (mk-arr (mk-tvar 'β)
                                       (mk-arr (mk-arr (mk-tvar 'A)
                                                       (mk-arr (mk-tvar 'β)
                                                               (mk-tvar 'β)))
                                               (mk-tvar 'β)))))
        (opal:check (mk-λ 'A
                          (mk-∀ 'β
                                (mk-arr (mk-var 'β)
                                        (mk-arr (mk-arr (mk-var 'A)
                                                        (mk-arr (mk-var 'β)
                                                                (mk-var 'β)))
                                                (mk-var 'β)))))
                    (mk-karr τ τ)
                    +empty-env+))

    ;; w : int
    (is α= (mk-val 2)
           (opal:check (mk-val 2) int-type +empty-env+))

    ;; (λ x. 2) : int → int
    (is α= (mk-mλ 'x int-type (mk-val 2))
        (opal:check (mk-λ 'x (mk-val 2))
                    (mk-arr int-type int-type)
                    +empty-env+))

    ;; (Λ a. λ x. x) : ∀ a. a → a
    (is α= (mk-abs 'a τ (mk-mλ 'x (mk-tvar 'a) (mk-mvar 'x)))
        (opal:check (mk-abs 'a (mk-λ 'x (mk-var 'x)))
                    (mk-∀ 'a (mk-arr (mk-tvar 'a) (mk-tvar 'a)))
                    +empty-env+))

    ;; (σ (x ≜ 2) (y ≜ 3)) : (Σ (x : int) (y : int))
    (is α= (mk-struct (entries
                       (mk-decl 'x int-type)
                       (mk-def 'x (mk-val 2))
                       (mk-decl 'y int-type)
                       (mk-def 'y (mk-val 3))))
        (opal:check
         (mk-struct (entries
                     (mk-def 'x (mk-val 2))
                     (mk-def 'y (mk-val 3))))
         (mk-sig (entries
                  (mk-decl 'x int-type)
                  (mk-decl 'y int-type)))
         +empty-env+))

    (is α=
        (mk-app (mk-mλ 'x int-type (mk-mvar 'x))
                (mk-val 2))
        (opal:check (mk-app (mk-λ 'x int-type (mk-var 'x))
                            (mk-val 2))
                    int-type
                    +empty-env+))

    ;; kind checking

    (is α=
        (mk-sig (entries (mk-decl 'x int-type)
                      (mk-decl 'y int-type)))
        (opal:check (mk-sig (entries (mk-decl 'x int-type)
                                  (mk-decl 'y int-type)))
                    τ
                    +empty-env+)))

  (define-test type-infer
    ;; infer 2 = int
    (is α=
        int-type
        (car (opal:infer (mk-val 2) +empty-env+)))

    ;; infer int = τ
    (is α= τ (car (opal:infer int-type +empty-env+)))

    ;; infer (λ x : int. x) = int → int
    (is α=
        (mk-arr int-type int-type)
        (car (opal:infer (mk-λ 'x int-type (mk-var 'x)) +empty-env+)))

    ;; infer (λ x : int. x) 2 = int
    (is α=
        int-type
        (car (opal:infer (mk-app (mk-λ 'x int-type (mk-var 'x)) (mk-val 2)) +empty-env+)))

    ;; infer (Λ a. λ x : a. x) = ∀ a. a → a
    (is α=
        (mk-∀ 'a (mk-arr (mk-tvar 'a) (mk-tvar 'a)))
        (car (opal:infer (mk-abs 'a (mk-λ 'x (mk-var 'a) (mk-var 'x))) +empty-env+)))

    ;; infer (Λ a. λ x : a. x) int = int → int
    (is α=
        (mk-arr int-type int-type)
        (car (opal:infer
         (mk-app (mk-abs 'a (mk-λ 'x (mk-var 'a) (mk-var 'x))) int-type) 
         +empty-env+))
        )

    ;; infer (σ (x=2) (y:int) (y≜2)) = Σ (x : int) (y : int)
    (is α=
        (car (opal:infer
         (mk-struct
          (entries (mk-def 'x (mk-val 2))
                (mk-decl 'y int-type)
                (mk-def 'y (mk-val 3))))
         +empty-env+))
        (mk-sig
         (entries
          (mk-decl 'x int-type)
          (mk-decl 'y int-type))))

    ;; infer (σ (x ≜ 2) (y ≜ x)) = Σ (x : int) (y : int)
    (is α=
        (car (opal:infer
         (mk-struct
          (entries (mk-def 'x (mk-val 2))
                (mk-def 'y (mk-var 'x))))
         +empty-env+))
        (mk-sig
         (entries
          (mk-decl 'x int-type)
          (mk-decl 'y int-type))))

    (is α=
        (car (opal:infer
         (mk-proj 'x (mk-struct (entries (mk-def 'x (mk-val 2)))))
         +empty-env+))
        int-type))

  (define-test type-reduction
      (α= (opal::ty-reduce int-type) int-type)

      (α= (opal::ty-reduce (mk-tapp (mk-∀ 'a (mk-var 'a)) int-type)) 
          int-type)))
