(in-package :sigil-tests)

(defparameter int-type (make-instance
                        'sigil::native-type
                        :native-type 'integer))

(define-test equality
  :parent all

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
        (make-instance 'sigil::native-type
                       :native-type 'integer))
    (is α=
        (mk-arr int-type int-type)
        (mk-arr int-type int-type))

    (is α=
        (mk-app (mk-val 2) (mk-val 2))
        (mk-app (mk-val 2) (mk-val 2)))

    (is α=
        (mk-app (mk-tvar 'a) (mk-native 'integer))
        (mk-app (mk-tvar 'a) (mk-native 'integer)))

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
          (mk-def 'y (mk-abs 'a (mk-λ 'x (mk-var 'a) (mk-var 'x)))))))

    (is α=
        (mk-induct
         'I
         (mk-kind)
         (list
          (mk-decl 'int-literal (mk-arr int-type (mk-tvar 'I)))
          (mk-decl 'constant (mk-tvar 'I))))
        (mk-induct
         'I
         (mk-kind)
         (list
          (mk-decl 'int-literal (mk-arr int-type (mk-tvar 'I)))
          (mk-decl 'constant (mk-tvar 'I))))))

  (define-test type-alpha-inequality
    (isnt α=
        (mk-∀ 'a (mk-var 'a))
        (mk-∀ 'a (mk-var 'b)))

    (isnt α=
        (mk-∀ 'b (mk-var 'a))
        (mk-∀ 'b (mk-var 'b)))

    (isnt α= int-type
        (make-instance 'sigil::native-type
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
                 (mk-decl 'y (mk-∀ 'b (mk-var 'b)))))))

  (define-test type-alpha-comparison
     (is α<=
         (mk-arr int-type (mk-var 'a))
         (mk-arr int-type (mk-var 'a)))
    
    (is α<=
        (mk-∀ 'a (mk-arr int-type (mk-var 'a)))
        (mk-∀ 'b (mk-arr int-type (mk-var 'b))))

    (is α<=
        (mk-tapp (mk-tvar 'a) (mk-tvar 'b))
        (mk-tapp (mk-tvar 'a) (mk-tvar 'b)))

    (is α<=
        (mk-∀ 'a (mk-tapp (mk-tvar 'x) (mk-tvar 'a)))
        (mk-∀ 'b (mk-tapp (mk-tvar 'x) (mk-tvar 'b))))

    (is α<=
        (mk-induct
         'I
         (mk-kind)
         (list
          (mk-decl 'int-literal (mk-arr int-type (mk-tvar 'I)))
          (mk-decl 'constant (mk-tvar 'I))))
        (mk-induct
         'I
         (mk-kind)
         (list
          (mk-decl 'int-literal (mk-arr int-type (mk-tvar 'I)))
          (mk-decl 'constant (mk-tvar 'I)))))

    (is α<=
        (mk-induct
         'I
         (mk-kind)
         (list
          (mk-decl 'int-literal (mk-arr int-type (mk-tvar 'I)))
          (mk-decl 'constant (mk-tvar 'I))))
        (mk-induct
         'B
         (mk-kind)
         (list
          (mk-decl 'int-literal (mk-arr int-type (mk-tvar 'B)))
          (mk-decl 'constant (mk-tvar 'B)))))

    (is α<=
        (mk-induct
         'I
         (mk-karr (mk-kind) (mk-kind))
         (list
          (mk-decl 'any
           (mk-abs 'a (mk-arr (mk-tvar 'a) (mk-tapp (mk-tvar 'I) (mk-tvar 'a)))))))
        (mk-induct
         'I
         (mk-karr (mk-kind) (mk-kind))
         (list
          (mk-decl 'any
           (mk-abs 'a (mk-arr (mk-tvar 'a) (mk-tapp (mk-tvar 'I) (mk-tvar 'a))))))))

    (is α<=
        (mk-karr (mk-kind) (mk-kind))
        (mk-karr (mk-kind) (mk-kind)))))


