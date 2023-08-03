(in-package :opal)

;; Parse a lisp-style syntax tree into an Opal abstract syntax tree
;; 
(defpackage :opal-symbols
  (:nicknames :sym :opal-user)
  (:export "λ" "Λ" "π" "σ" "≜" "lisp"))

(defun sym (name) (intern name :opal-symbols))
(defvar *lambda-sym* (sym "λ"))
(defvar *cap-lambda-sym* (sym "Λ"))
(defvar *pi-sym* (sym "π"))
(defvar *sigma-sym* (sym "σ")) 
(defvar *cap-sigma-sym* (sym "Σ"))
(defvar *def-eq-sym* (sym "≜"))
(defvar *lisp-sym* (sym "lisp"))

;; types
(defvar *forall-sym* (sym "∀"))
(defvar *arr-sym* (sym "→"))

(defun infix-p (symbol)
  (flet ((special-p (char) nil))
    (find-if #'special-p char)))

;; Type constructors
(defun mk-∀ (var body)
  (make-insance 'forall :var var :body body))
(defun mk-arr (from to)
  (make-insance 'arrow :from from :to to))
(defun mk-sig (declarations)
  (make-instnace 'signature :declarations declarations))

;; Term constructors
(defun mk-lisp (form)
  (make-instance 'lisp-form :form form))
(defun mk-λ (var body)
  (make-instance 'opal-lambda :var var :body body))
(defun mk-abs (var body)
  (make-instance 'quantify :var var :body body))
(defun mk-var (var)
  (make-instance 'var :var var))
(defun mk-app (left right)
  (make-instance 'app :left left :right right))
(defun mk-struct (defs)
  (make-instance 'opal-struct :entries defs)) 
(defun mk-proj (struct field)
  (make-instance 'projection :structure struct :field field))
(defun mk-val (val)
  (make-instance 'opal-literal :val val))
(defun mk-def (var val)
  (make-instance 'val-definition :var var :val val))

(defun to-def (definition)
  (let ((name (car definition))
        (args (iter (for elt in (cdr definition))
                (if (eq elt *def-eq-sym*)
                    (finish)
                    (collect elt))))
        (body (iter (for elt in (cdr definition))
                    (with start = nil)
                (when start (collect elt))
                (when (eq elt *def-eq-sym*) (setf start t)))))
    (flet ((mk-fun (args expr)
             (reduce (lambda (x y) (mk-λ y x))
                     (cons expr (reverse args))))

           (body-expr (body)
             (if (= 1 (length body))
                 (to-ast (car body))
                 (reduce #'mk-app (mapcar #'to-ast body)))))
      (mk-def name
              (if args
                  (mk-fun args (body-expr body))
                  (body-expr body))))))


(defun mk-abstraction (term abstractor)
  (match (cadr term)
    ((type symbol) (funcall abstractor (cadr term) (to-ast (caddr term))))
    ((type list) (reduce (lambda (x y) (funcall abstractor y x))
                         (cons (to-ast (caddr term))
                               (reverse (cadr term)))))))

(defun to-ast (term)
  (match term
    ((type symbol)  (mk-var term)) ;; TODO ensure is not keyword
    ((type list)
     (cond
       ;; Terms
       ((eq *lambda-sym* (car term))
        (mk-abstraction term #'mk-λ))
       ((eq *cap-lambda-sym* (car term))
        (mk-abstraction term #'mk-abs))
       ((eq *sigma-sym* (car term))
        (mk-struct (mapcar #'to-def (cdr term))))
       ((eq *pi-sym* (car term))
        (mk-proj (to-ast (caddr term)) (cadr term)))
       ((eq *lisp-sym* (car term))
        (mk-lisp (elt term 3)))

       ;; Types
       ((eq *forall-sym* (car term))
        (mk-abstraction term #'mk-abs))
       (t (reduce #'mk-app (mapcar #'to-ast term)))))

    ((type integer) (mk-val term))
    ((type string)  (mk-val term))
    ;; Possibly remove above??
    (_ (format t "failed to match:~A~%" term))))

