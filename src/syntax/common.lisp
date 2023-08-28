(in-package :silica)

(defgeneric show ())

(defgeneric α= ())

(defgeneric α= ())

(defun α>= (l r &optional shadow rename)
  (α<= r l
       (li:map (lambda (c) (cons (cdr c) (car c))) renamings)
       (cons (cdr shadowed) (car shadowed))))
