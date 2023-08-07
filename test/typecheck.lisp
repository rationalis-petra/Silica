(in-package :opal.tests)

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

    ;; ∀ a. a ◂ τ → τ
    (is α= (mk-∀ 'a (mk-var 'a))
        (opal:check (mk-∀ 'a (mk-var 'a)) (mk-karr τ τ) +empty-env+))

    ;; w : int
    (is α= (mk-val 2)
           (opal:check (mk-val 2) int-type +empty-env+))

    ;; (λ x. 2) : int → int
    (is α= (mk-λ 'x int-type (mk-val 2))
        (opal:check (mk-λ 'x (mk-val 2))
                    (mk-arr int-type int-type)
                    +empty-env+))

    ;; (Λ a. λ x. x) : ∀ a. a → a
    (is α= (mk-abs 'a τ (mk-λ 'x (mk-var 'a) (mk-var 'x)))
        (opal:check (mk-abs 'a (mk-λ 'x (mk-var 'x)))
                    (mk-∀ 'a (mk-arr (mk-var 'a) (mk-var 'a)))
                    +empty-env+))

    ;; (σ (x ≜ 2) (y ≜ 3)) : (Σ (x : int) (y : int))
    (is α= (mk-struct (list
                       (mk-decl 'x int-type)
                       (mk-def 'x (mk-val 2))
                       (mk-decl 'y int-type)
                       (mk-def 'y (mk-val 3))))
        (opal:check
         (mk-struct (list (mk-def 'x (mk-val 2))
                          (mk-def 'y (mk-val 3))))
         (mk-sig (list (mk-decl 'x int-type)
                       (mk-decl 'y int-type)))
         +empty-env+))

    (is α=
        (mk-app (mk-λ 'x int-type (mk-var 'x))
                (mk-val 2))
        (opal:check (mk-app (mk-λ 'x int-type (mk-var 'x))
                            (mk-val 2))
                    int-type
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
        (car (opal:infer (mk-app (mk-λ 'x int-type (mk-var 'x)) (mk-val 2)) +empty-env+))
        int-type)

    ;; infer (Λ a. λ x : a. x) = ∀ a. a → a
    (is α=
        (car (opal:infer (mk-abs 'a (mk-λ 'x (mk-var 'a) (mk-var 'x))) +empty-env+) ) 
        (mk-∀ 'a (mk-arr (mk-var 'a) (mk-var 'a))))

    ;; infer (Λ a. λ x : a. x) int = int → int
    (is α=
        (car (opal:infer
         (mk-app (mk-abs 'a (mk-λ 'x (mk-var 'a) (mk-var 'x))) int-type) 
         +empty-env+))
        (mk-arr int-type int-type)
        )

    ;; infer (σ (x=2) (y:int) (y≜2)) = Σ (x : int) (y : int)
    (is α=
        (car (opal:infer
         (mk-struct
          (list (mk-def 'x (mk-val 2))
                (mk-decl 'y int-type)
                (mk-def 'y (mk-val 3))))
         +empty-env+))
        (mk-sig
         (list
          (mk-decl 'x int-type)
          (mk-decl 'y int-type))))

    ;; infer (σ (x ≜ 2) (y ≜ x)) = Σ (x : int) (y : int)
    (is α=
        (car (opal:infer
         (mk-struct
          (list (mk-def 'x (mk-val 2))
                (mk-def 'y (mk-var 'x))))
         +empty-env+))
        (mk-sig
         (list
          (mk-decl 'x int-type)
          (mk-decl 'y int-type))))

    (is α=
        (car (opal:infer
         (mk-proj 'x (mk-struct (list (mk-def 'x (mk-val 2)))))
         +empty-env+))
        int-type))

  (define-test type-reduction
      (α= (opal::ty-reduce int-type) int-type)

      (α= (opal::ty-reduce (mk-tapp (mk-∀ 'a (mk-var 'a)) int-type)) 
          int-type)))
