(defpackage :silica/util
  (:use :cl :iterate)
  (:export :app-curry))

(in-package :silica/util)

(defmacro app-curry (func &rest args)
  (iter (for arg in args)
    (accumulate arg by (lambda (arg body) `(funcall ,body ,arg))
                initial-value func)))

(in-package :silica)

(declaim (ftype (function (string string &rest list) t) silica-val))
(defun silica-val (package module &rest path)
  (let* ((pkg (gethash (sym package) *packages*))
         (module (gethash (sym module) (modules pkg))))
    (iter (for name in path)
      (with val = (lisp-val module))
      (setf val (gethash (sym name) val))
      (finally (return val)))))

(defun run-main (&rest path)
  (funcall (apply #'silica-val (append path '("main"))) t))

(defparameter *diagnostic* nil)

(defun process-module (name imports exports defs)
  (declare (ignore name imports exports))
  (let* ((infixified (infixify defs))
         (ast (li:map (alexandria:compose
                       (lambda (def) (mk-entry (var def) def))
                       #'to-def)
                      infixified))
         (result (infer (mk-struct ast) +empty-env+))
         (expr (reify (cdr result) nil))
         )

    (when *diagnostic*
      (format t "infixified: ~A~%" infixified)
      (format t "ast: ~A~%" ast)
      (format t "type: ~A~%" (car result))
      (format t "processed: ~A~%" (cdr result))
      (format t "expr: ~A~%" expr))
    expr))

(named-readtables:defreadtable silica:classic
  (:merge :standard)
  (:case :preserve))

(in-package :silica-user)

;; TODO: add typechecking...
(cl:defmacro |module| (name cl:&body defs)
  `(cl:setf (cl:gethash (cl:quote ,name) silica:*silica-modules*)
            ,(silica::process-module name () () defs)))

  ;; `(cl:setf (cl:gethash (cl:quote ,name) silica:*silica-modules*)
  ;;           ,(silica:reify
  ;;             (silica::mk-struct (cl:mapcar #'silica:to-def defs))
  ;;             cl:nil)))

(cl:unless (cl:boundp (cl:quote |τ|))
  (cl:defconstant |τ|
    (cl:make-instance 'silica:kind-type)))

;; Grammar
;; infix operators → all same fixity 
;; Left vs Right?
;; 
;; 

;; (defun parse-silica-lisp ())
