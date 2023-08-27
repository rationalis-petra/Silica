(in-package :opal)

(defgeneric show ())

(defgeneric α= ())

(defgeneric α<= ())

(defun α>= (l r &optional shadow rename)
  (<= l r shadow rename))
