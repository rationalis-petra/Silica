(in-package :opal)

;; Parse a lisp-style syntax tree into an Opal abstract syntax tree
;; 
(defpackage :opal-symbols
  (:nicknames :sym :opal-user)
  (:export "λ" "Λ" "π" "σ" "Σ" "∀" "→" "≜" "◂" "lisp"))

(defun sym (name) (intern name :opal-symbols))
(defvar *lambda-sym* (sym "λ"))
(defvar *cap-lambda-sym* (sym "Λ"))
(defvar *pi-sym* (sym "."))
(defvar *sigma-sym* (sym "σ")) 
(defvar *cap-sigma-sym* (sym "Σ"))
(defvar *def-sym* (sym "≜"))
(defvar *ann-sym* (sym "◂"))
(defvar *native-sym* (sym "native"))
(defvar *lisp-sym* (sym "lisp"))
(defvar *if-sym* (sym "if"))

;; types
(defvar *forall-sym* (sym "∀"))
(defvar *arrow-sym* (sym "→"))

;; kinds
(defvar *kind-sym* (sym "τ"))
(defvar *kind-t* (mk-kind))

(defun special? (char)
  (and 
   (member (cl-unicode:general-category char)
           (list "Pc" "Pd" "Ps" "Pe" "Pi" "Pf" "Po"
                 "Sm" "Rc" "Sk" "So")
           :test #'equal)
   (not (eq char #\∀))
   (not (eq char #\∃))))

(declaim (ftype (function (t) boolean) infix?))
(defun infix? (val)
  (when (typep val 'symbol)
    (every #'special? (string val))))


(declaim (ftype (function (list) (or opal-declaration opal-definition)) to-def))
(defun to-def (definition)
  "Produce a definition from a boi"
  (let ((name (if (listp (cadr definition))
                  (caadr definition)
                  (cadr definition)))
        (body (caddr definition)))
    (flet ((mk-fun (args expr)
             (reduce (lambda (x y) (mk-λ (get-var y) x))
                     (cons expr (reverse args)))))
      (cond 
        ((eq *def-sym* (car definition))
         (mk-def name
                 (if (listp (cadr definition))
                     (mk-fun (cdadr definition) (to-ast body))
                     (to-ast body))))
        ;; TODO: fix me!
        ((eq *ann-sym* (car definition))
         (mk-decl name
                  (if (listp (cadr definition))
                      (mk-fun (cdadr definition) (to-ast body))
                      (to-ast body))))
        (t (error "bad def!"))))))

(defun get-var (x)
  (cond
    ((typep x 'symbol) x)
    ((and (typep x 'list) (= (length x) 1))
     (get-var (car x)))
    (t (error (format nil "malformed var, reached:~A" x)))))

(defun mk-abstraction (term abstractor)
  (flet ((abstract-decl? (func decl body)
           (cond
             ((typep decl 'symbol) (funcall func decl body))
             ((and (typep decl 'list) (= (length decl) 1))
              (funcall func (get-var decl) body))
             (t
              (let ((decl2 (to-def decl)))
                (assert (typep decl2 'opal-declaration))
                (funcall func (var decl2) (ann decl2) body))))))
  (match (cadr term)
    ((type symbol) (funcall abstractor (cadr term) (to-ast (caddr term))))
    ((type list)
     (reduce (lambda (x y) (abstract-decl? abstractor y x))
             (cons (to-ast (caddr term))
                   (reverse (cadr term))))))))

(defun to-ast (term)
  "Take a raw concrete ast (represented as a list), and convert it into an opal
syntax-tree."
  (match term
    ((type keyword)
     (mk-val term))
    ((type symbol)
     (if (eq *kind-sym* term)
         *kind-t*
         (mk-var term))) ;; TODO ensure is not keyword
    ((type list)
     (cond
       ;; Terms
       ((eq *lambda-sym* (car term))
        (mk-abstraction term #'mk-λ))
       ((eq *cap-lambda-sym* (car term))
        (mk-abstraction term #'mk-abs))
       ((eq *sigma-sym* (car term))
        (mk-struct
         (mapcar (lambda (def) (make-instance 'entry :var (var def) :binder def))
                 (mapcar #'to-def (cdr term)))))
       ((eq *cap-sigma-sym* (car term))
        (mk-sig
         (mapcar (lambda (def) (make-instance 'entry :var (var def) :binder def))
                 (mapcar #'to-def (cdr term)))))
       ((eq *pi-sym* (car term))
        (mk-proj (get-var (caddr term)) (to-ast (cadr term))))
       ((eq *if-sym* (car term))
        (mk-if (to-ast (elt term 1))
               (to-ast (elt term 2))
               (to-ast (elt term 3))))
       ((eq *lisp-sym* (car term))
        (mk-lisp (to-ast (elt term 1)) (elt term 3)))

       ;; Types
       ((eq *native-sym* (car term))
        (mk-native (cadr term)))
       ((eq *forall-sym* (car term))
        (mk-abstraction term #'mk-∀))
       ((eq *arrow-sym* (car term))
        (let ((left (to-ast (cadr term)))
              (right (to-ast (caddr term))))
          (typecase (cons left right)
            ((cons opal-type opal-type) (mk-arr left right))
            ((cons kind kind) (mk-karr left right))
            (t (error "→ expects either two kinds or two types")))))
       (t
        (reduce #'mk-app (mapcar #'to-ast term)))))

    ((type number) (mk-val term))
    ((type string) (mk-val term))
    ;; Possibly remove above??
    (_ (error (format nil "failed to match:~A" term)))))


(defun point-tighten (term)
  "'Tighten' expressions containing a point, e.g. convert a . b + c to
(a . b) + c, or f . g h to (f . g) h. This ensures that '.' will always be
  treated with the highest priority."
  term)

(defun infixify (term)
  "Take raw concrete syntax tree (represented as a list), and transform it so
that infix operations are moved prefix. For example '(2 + a) is converted to
'(+ 2 a), and '(a + b * c) is converted to '(+ a (* b c))."
  (cond
    ((and (typep term 'list) (eq (car term) *lisp-sym*))
     (cons (car term) (cons (infixify (cadr term)) (cddr term))))

    ;; (n) → (infix n)
    ((and (typep term 'list) (= 1 (length term)))
     (mapcar #'infixify term))

    ((typep term 'list)
     (flet ((proc-head (head)
              (if (= 1 (length head))
                  (infixify (car head))
                  (mapcar #'infixify (reverse head))))
            (proc-tail (tail)
              (if (= 1 (length tail))
                  (infixify (car tail)))
              (infixify tail)))

       (iter (for elt in term)
             (for i from 1)
             (with vals = nil)

         (if (infix? elt)
             (return 
               (if vals
                   (list elt (proc-head vals) (proc-tail (nthcdr i term)))
                   (list elt (proc-tail (nthcdr i term)))))
             (push elt vals))
         (finally (return (proc-head vals))))))
    (t term)))


(defun module-header (term)
  "Parse a module header"
  (flet ((parse-import-list (import-list)))

  ))
