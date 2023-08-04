(in-package :opal.tests)

(defparameter int-type (make-instance
                        'opal::native-type
                        :native-type 'integer))

(define-test equality
  (define-test type-alpha-equality
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
        (mk-∀ 'a (mk-arr int-type (mk-var 'a)))
        (mk-∀ 'b (mk-arr int-type (mk-var 'b))))

    (is α=
        (mk-sig (list (mk-decl 'x int-type)))
        (mk-sig (list (mk-decl 'x int-type))))

    (is α=
        (mk-sig (list
                 (mk-decl 'x int-type)
                 (mk-decl 'y (mk-∀ 'a (mk-var 'a)))))
        (mk-sig (list
                 (mk-decl 'x int-type)
                 (mk-decl 'y (mk-∀ 'b (mk-var 'b)))))))

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
        (mk-sig (list (mk-decl 'x int-type)))
        (mk-sig (list (mk-decl 'y int-type))))

    (isnt α=
        (mk-sig (list
                 (mk-decl 'x (mk-arr (mk-var 'a) int-type))
                 (mk-decl 'y (mk-∀ 'a (mk-var 'a)))))
        (mk-sig (list
                 (mk-decl 'x int-type)
                 (mk-decl 'y (mk-∀ 'b (mk-var 'b))))))))


