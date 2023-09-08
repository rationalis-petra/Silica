(in-package :silica)

(defgeneric show (value))

(defgeneric α= (left right &optional shadow rename))

(defgeneric α<= (left right &optional shadow rename))

(defun α>= (l r &optional shadow rename)
  (α<= r l
       (li:map (lambda (c) (cons (cdr c) (car c))) renamings)
       (cons (cdr shadowed) (car shadowed))))
