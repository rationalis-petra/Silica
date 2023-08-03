(in-package :opal)

;; we use biridrectional typechecking
;; i.e. 1x check method, 1x infer method
(defgeneric check (term type env)
  (:method ((term lisp-form) type env)
    (if (equal type (form-type))
        term
        (error (format nil "Types not equal: ~A and ~A~%" type form-type))))

  (:method ((term opal-literal) (type native-type) env)
    (if (typep (val term) (lisp-type type))
        term
        (error (format nil "Term ~A does not have type ~A~%" term type))))

  (:method ((term opal-lambda) (type arrow) env)
    (flet ((body-check (from to)
             (check (body term) from (acons (var term) to) env)))
      (if (slot-boundp 'var-type term)
          (if (equal (from type) (var-type term))
              (body-check (var-type term) (to type))
              (error "Declared function argument type doesn't match actual
    type"))
          (body-check (from type) (to type)))))

  (:method ((term term) (type opal-type) env)
    (error (format nil "Failed to typecheck term ~A as type ~A~%" type))))


(defgeneric infer (term env)
  (:method (term env)
    (error (format nil "Cannot infer type of term ~A~%" term))))
