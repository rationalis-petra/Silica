(defpackage silica/context
  (:use :cl :iter :alexandria)
  (:nicknames :ctx)
  (:export
   :context
   :make-from
   :empty
   :lookup
   :hide))
(in-package :ctx)

(defstruct context base shadow)

(declaim (ftype (function (hash-table) context) make-from))
(defun make-from (base)
  (make-context :base base :shadow nil))

(declaim (type context +empty+))
(defvar +empty+ (make-context :base (ht:empty) :shadow nil))

(declaim (ftype (function (symbol context) context) hide))
(defun hide (var ctx)
  (make-context
   :base (context-base ctx)
   :shadow (cons var (context-shadow ctx))))

(declaim (ftype (function (symbol context) t) hide))
(defun lookup (var ctx)
  (unless (member var (context-shadow ctx)))
  (gethash var (context-base ctx)))


