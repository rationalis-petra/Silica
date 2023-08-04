
(named-readtables:defreadtable opal:classic
  (:merge :standard)
  (:case :preserve))

(in-package :opal-user)

(cl:defmacro |module| (name cl:&body defs)
  `(cl:setf (cl:gethash (cl:quote ,name) opal:*opal-modules*)
            ,(opal:reify
              (opal::mk-struct (cl:mapcar #'opal:to-def defs))
              cl:nil)))

(cl:unless (cl:boundp (cl:quote |τ|))
  (cl:defconstant |τ|
    (cl:make-instance 'opal:kind-type)))

;; Grammar
;; infix operators → all same fixity 
;; Left vs Right?
;; 
;; 

;; (defun parse-opal-lisp ())
