(in-package :silica)

;; Parse a lisp-style syntax tree into an silica abstract syntax tree
;; 
(defpackage :silica-symbols
  (:nicknames :sym :silica-user)
  (:export "λ" "Λ" "π" "σ" "Σ" "∀" "→" "≜" "◂" "lisp"))

(defun sym (name) (intern name :silica-symbols))

(defvar *arrow-sym* (sym "→"))
(defvar *forall-sym* (sym "∀"))
(defvar *cap-sigma-sym* (sym "Σ"))
(defvar *cap-phi-sym* (sym "Φ"))
(defvar *native-sym* (sym "native"))

(defvar *lambda-sym* (sym "λ"))
(defvar *cap-lambda-sym* (sym "Λ"))
(defvar *sigma-sym* (sym "σ")) 
(defvar *phi-sym* (sym "φ"))
(defvar *lisp-sym* (sym "lisp"))

(defvar *if-sym* (sym "if"))
(defvar *is-sym* (sym "is"))

(defvar *dot-sym* (sym "."))
(defvar *ann-sym* (sym "⮜")) 
(defvar *def-sym* (sym "≜"))
(defvar *kind-sym* (sym "τ"))

(defvar *wildcard-sym* (sym "_"))
(defvar *pkg-wildcard-sym* (sym "…"))

(defun inductive-sym? (sym)
  (eq sym *cap-phi-sym*))
(defun match-sym? (sym)
  (eq sym *phi-sym*))
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
(defun is-sym? (sym)
  (eq sym *is-sym*))
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
  (or (inductive-sym? sym)
      (match-sym? sym)
      (func-sym? sym)
      (arrow-sym? sym)
      (abs-sym? sym)
      (forall-sym? sym)
      (proj-sym? sym)
      (struct-sym? sym)
      (sig-sym? sym)
      (kind-sym? sym)
      (def-sym? sym)
      (ann-sym? sym)
      (is-sym? sym)
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


(declaim (ftype (function (list) (or silica-declaration silica-definition)) to-def))
(defun to-def (definition)
  "Produce a definition from a boi"
  (let ((name (if (listp (cadr definition))
                  (caadr definition)
                  (cadr definition)))
        (body (caddr definition)))
    (flet ((mk-fun (args expr)
             (reduce (lambda (x y)
                       (match (def-var y)
                         ((pair (fst :arg) (snd s)) (mk-λ s x))
                         ((pair (fst :typ) (snd s)) (mk-abs s x))
                         (val
                          (error
                           (format nil "failed to match def-var output: ~A" val)))))
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

(declaim (ftype (function ((or symbol list)) pair) def-var))
(defun def-var (x)
  (match x
    ((type symbol)
     (pair :arg x))
    ((list inner)
     (def-var inner))
    ((list 'sym::|⟨⟩| sym)
     (pair :typ (get-var sym)))
    (t (error (format nil "malformed var, reached:~A" x)))))

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
                (assert (typep decl2 'silica-declaration))
                (funcall func (var decl2) (ann decl2) body))))))
  (match (cadr term)
    ((type symbol) (funcall abstractor (cadr term) (to-ast (caddr term))))
    ((type list)
     (reduce (lambda (x y) (abstract-decl? abstractor y x))
             (cons (to-ast (caddr term))
                   (reverse (cadr term))))))))

(defun to-ast (term)
  "Take a raw concrete ast (represented as a list), and convert it into an silica
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
       ((func-sym? (first term))
        (mk-abstraction term #'mk-λ))
       ((abs-sym? (first term))
        (mk-abstraction term #'mk-abs))
       ((inductive-sym? (first term))
        (make-instance 'inductive-type
         :constructors (li:map #'to-def (rest term))))
       ((match-sym? (first term))
        (mk-match
         (to-ast (elt term 1))
         (li:map #'to-match-clause (li:drop 2 term))))
       ((is-sym? (first term))
        (mk-is (to-ast (elt term 1)) (to-ast (elt term 2))))
       ((struct-sym? (first term))
        (mk-struct
         (li:map (lambda (def)
                   (make-instance 'entry :var (var def) :binder def))
                 (li:map #'to-def (rest term)))))
       ((sig-sym? (first term))
        (mk-sig
         (mapcar (lambda (def) (make-instance 'entry :var (var def) :binder def))
                 (mapcar #'to-def (rest term)))))
       ((proj-sym? (first term))
        (mk-proj (get-var (caddr term)) (to-ast (cadr term))))
       ((cond-sym? (first term))
        (mk-if (to-ast (elt term 1))
               (to-ast (elt term 2))
               (to-ast (elt term 3))))
       ((lisp-sym? (first term))
        (mk-lisp (to-ast (elt term 1)) (elt term 3)))

       ;; Types
       ((native-sym? (first term))
        (mk-native (cadr term)))
       ((eq *forall-sym* (first term))
        (mk-abstraction term #'mk-∀))
       ((eq *arrow-sym* (first term))
        (let ((left (to-ast (cadr term)))
              (right (to-ast (caddr term))))
          (typecase (cons left right)
            ((cons silica-type silica-type) (mk-arr left right))
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
;;   (module.submodule.(…)
;;    math.(x y z)))
;; 
;; (:imports
;;   (module submodule :wildcard)
;;   (math (x y z)))

(defun to-match-clause (term)
  (unless (eq *arrow-sym* (first term))
    (error (format nil "Match clause expects →, got ~A" (first term))))

  (labels 
      ((process-pattern (pattern)
         (typecase pattern
           (list
            (if (= (length pattern) 1)
                (process-pattern (first pattern))
                (mapcar #'process-pattern pattern)))
           (t pattern))))

    (make-instance
     'match-clause
     :pattern (process-pattern (elt term 1))
     :body (to-ast (elt term 2)))))
