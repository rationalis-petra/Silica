(in-package :opal.tests)

(defparameter int-type (make-instance
                        'opal::native-type
                        :native-type 'integer))

(defun entries (&rest terms)
  (mapcar (lambda (term) (mk-entry (var term) term)) terms))

(define-test equality
  (define-test type-alpha-equality
    (is α= (mk-kind) (mk-kind))

    (is α=
        (mk-karr (mk-kind) (mk-kind))
        (mk-karr (mk-kind) (mk-kind)))

    (is α=
        (mk-∀ 'a (mk-var 'a))
        (mk-∀ 'a (mk-var 'a)))

    (is α=
        (mk-∀ 'a (mk-var 'a))
        (mk-∀ 'b (mk-var 'b)))

    (is α= int-type
        (make-instance 'opal::native-type
                       :native-type 'integer))
    (is α=
        (mk-arr int-type int-type)
        (mk-arr int-type int-type))

    (is α=
        (mk-app (mk-val 2) (mk-val 2))
        (mk-app (mk-val 2) (mk-val 2)))

    (is α=
        (mk-∀ 'a (mk-arr int-type (mk-var 'a)))
        (mk-∀ 'b (mk-arr int-type (mk-var 'b))))

    (is α=
        (mk-sig (entries (mk-decl 'x int-type)))
        (mk-sig (entries (mk-decl 'x int-type))))

    (is α=
        (mk-sig (entries
                 (mk-decl 'x int-type)
                 (mk-decl 'y (mk-∀ 'a (mk-var 'a)))))
        (mk-sig (entries
                 (mk-decl 'x int-type)
                 (mk-decl 'y (mk-∀ 'b (mk-var 'b))))))

    (is α=
        (mk-struct
         (entries
          (mk-decl 'x int-type)
          (mk-def 'x (mk-val 2))
          (mk-decl 'y (mk-∀ 'a (mk-arr (mk-var 'a) (mk-var 'a))))
          (mk-def 'y (mk-abs 'a (mk-λ 'x (mk-var 'a) (mk-var 'x))))))
        (mk-struct
         (entries
          (mk-decl 'x int-type)
          (mk-def 'x (mk-val 2))
          (mk-decl 'y (mk-∀ 'a (mk-arr (mk-var 'a) (mk-var 'a))))
          (mk-def 'y (mk-abs 'a (mk-λ 'x (mk-var 'a) (mk-var 'x))))))))

  (define-test type-alpha-inequality
    (isnt α=
        (mk-∀ 'a (mk-var 'a))
        (mk-∀ 'a (mk-var 'b)))

    (isnt α=
        (mk-∀ 'b (mk-var 'a))
        (mk-∀ 'b (mk-var 'b)))

    (isnt α= int-type
        (make-instance 'opal::native-type
                       :native-type 'symbol))

    (isnt α=
        (mk-arr (mk-var 'a) int-type)
        (mk-arr int-type int-type))

    (isnt α=
        (mk-∀ 'a (mk-arr int-type (mk-var 'a)))
        (mk-∀ 'b (mk-arr int-type (mk-var 'a))))

    (isnt α=
        (mk-sig (entries (mk-decl 'x int-type)))
        (mk-sig (entries (mk-decl 'y int-type))))

    (isnt α=
        (mk-sig (entries
                 (mk-decl 'x (mk-arr (mk-var 'a) int-type))
                 (mk-decl 'y (mk-∀ 'a (mk-var 'a)))))
        (mk-sig (entries
                 (mk-decl 'x int-type)
                 (mk-decl 'y (mk-∀ 'b (mk-var 'b))))))))


