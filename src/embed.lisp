(in-package :opal)

(defun run-main ()
  (funcall (gethash (sym "main") (gethash (sym "main") *opal-modules*))))

(defun get-opal-val (string)
  (gethash (sym string) (gethash (sym "main") *opal-modules*)))

(defvar *diagnostic* nil)

(defun process-module (name imports exports defs)
  (declare (ignore name imports exports))
  (let* ((infixified (infixify defs))
         (ast (mapcar #' to-def infixified))
         (type (infer (mk-struct ast) nil))
         (expr (reify (mk-struct ast) nil))
         )

    (when *diagnostic*
      (format t "infixified: ~A~%" infixified)
      (format t "ast: ~A~%" ast)
      (format t "type: ~A~%" type)
      (format t "expr: ~A~%" expr))
    expr))

(named-readtables:defreadtable opal:classic
  (:merge :standard)
  (:case :preserve))

(in-package :opal-user)

;; TODO: add typechecking...
(cl:defmacro |module| (name cl:&body defs)
  `(cl:setf (cl:gethash (cl:quote ,name) opal:*opal-modules*)
            ,(opal::process-module name () () defs)))

  ;; `(cl:setf (cl:gethash (cl:quote ,name) opal:*opal-modules*)
  ;;           ,(opal:reify
  ;;             (opal::mk-struct (cl:mapcar #'opal:to-def defs))
  ;;             cl:nil)))

(cl:unless (cl:boundp (cl:quote |τ|))
  (cl:defconstant |τ|
    (cl:make-instance 'opal:kind-type)))

;; Grammar
;; infix operators → all same fixity 
;; Left vs Right?
;; 
;; 

;; (defun parse-opal-lisp ())
