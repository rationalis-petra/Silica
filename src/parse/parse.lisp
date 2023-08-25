(in-package :sigil)

;; Parse a lisp-style syntax tree into an Sigil abstract syntax tree
;; 
(defpackage :sigil-symbols
  (:nicknames :sym :sigil-user)
  (:export "λ" "Λ" "π" "σ" "Σ" "∀" "→" "≜" "◂" "lisp"))

(defun sym (name) (intern name :sigil-symbols))

(defvar *lambda-sym* (sym "λ"))
(defvar *arrow-sym* (sym "→"))
(defvar *cap-lambda-sym* (sym "Λ"))
(defvar *forall-sym* (sym "∀"))
(defvar *dot-sym* (sym "."))
(defvar *sigma-sym* (sym "σ")) 
(defvar *cap-sigma-sym* (sym "Σ"))
(defvar *def-sym* (sym "≜"))
(defvar *ann-sym* (sym "⮜")) 
(defvar *native-sym* (sym "native"))
(defvar *lisp-sym* (sym "lisp"))
(defvar *if-sym* (sym "if"))
(defvar *kind-sym* (sym "τ"))

(defvar *wildcard-sym* (sym "_"))
(defvar *pkg-wildcard-sym* (sym "…"))

(defun func-sym? (sym)
  (eq sym *lambda-sym*))
(defun arrow-sym? (sym)
  (eq sym *arrow-sym*))
(defun abs-sym? (sym)
  (eq sym *cap-lambda-sym*))
(defun forall-sym? (sym)
  (eq sym *forall-sym*))
(defun proj-sym? (sym)
  (eq sym *dot-sym*))
(defun struct-sym? (sym)
  (eq sym *sigma-sym*))
(defun sig-sym? (sym)
  (eq sym *cap-sigma-sym*))
(defun def-sym? (sym)
  (eq sym *def-sym*))
(defun ann-sym? (sym)
  (eq sym *ann-sym*))
(defun cond-sym? (sym)
  (eq sym *if-sym*))
(defun native-sym? (sym)
  (eq sym *native-sym*))
(defun lisp-sym? (sym)
  (eq sym *lisp-sym*))
(defun kind-sym? (sym)
  (eq sym *kind-sym*))
(defun wildcard-sym? (sym)
  (eq sym *wildcard-sym*))
(defun pkg-wildcard-sym? (sym)
  (eq sym *pkg-wildcard-sym*))

(defun keyword-sym? (sym)
  (or (func-sym? sym)
      (arrow-sym? sym)
      (abs-sym? sym)
      (forall-sym? sym)
      (proj-sym? sym)
      (struct-sym? sym)
      (sig-sym? sym)
      (kind-sym? sym)
      (def-sym? sym)
      (ann-sym? sym)
      (cond-sym? sym)
      (native-sym? sym)
      (lisp-sym? sym)
      (wildcard-sym? sym)
      (pkg-wildcard-sym? sym)))

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


(declaim (ftype (function (list) (or sigil-declaration sigil-definition)) to-def))
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
                (assert (typep decl2 'sigil-declaration))
                (funcall func (var decl2) (ann decl2) body))))))
  (match (cadr term)
    ((type symbol) (funcall abstractor (cadr term) (to-ast (caddr term))))
    ((type list)
     (reduce (lambda (x y) (abstract-decl? abstractor y x))
             (cons (to-ast (caddr term))
                   (reverse (cadr term))))))))

(defun to-ast (term)
  "Take a raw concrete ast (represented as a list), and convert it into an sigil
syntax-tree."
  (match term
    ((type keyword)
     (mk-val term))
    ((type symbol)
     (cond
       ((eq *kind-sym* term) *kind-t*)
       ((keyword-sym? term)
        (error (format nil "keyword ~A occurred in bad location" term)))
       (t (mk-var term))))
    ((type list)
     (cond
       ;; Terms
       ((func-sym? (car term))
        (mk-abstraction term #'mk-λ))
       ((abs-sym? (car term))
        (mk-abstraction term #'mk-abs))
       ((struct-sym? (car term))
        (mk-struct
         (mapcar (lambda (def) (make-instance 'entry :var (var def) :binder def))
                 (mapcar #'to-def (cdr term)))))
       ((sig-sym? (car term))
        (mk-sig
         (mapcar (lambda (def) (make-instance 'entry :var (var def) :binder def))
                 (mapcar #'to-def (cdr term)))))
       ((proj-sym? (car term))
        (mk-proj (get-var (caddr term)) (to-ast (cadr term))))
       ((cond-sym? (car term))
        (mk-if (to-ast (elt term 1))
               (to-ast (elt term 2))
               (to-ast (elt term 3))))
       ((lisp-sym? (car term))
        (mk-lisp (to-ast (elt term 1)) (elt term 3)))

       ;; Types
       ((native-sym? (car term))
        (mk-native (cadr term)))
       ((eq *forall-sym* (car term))
        (mk-abstraction term #'mk-∀))
       ((eq *arrow-sym* (car term))
        (let ((left (to-ast (cadr term)))
              (right (to-ast (caddr term))))
          (typecase (cons left right)
            ((cons sigil-type sigil-type) (mk-arr left right))
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

;; module header output:
;; (import
;;   (module.submodule._
;;    math.(x y z)))
;; 
;; (:imports
;;   (module submodule :wildcard)
;;   (math (x y z)))



