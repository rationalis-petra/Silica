(defpackage :optics
  (:use :cl :iter :lang-extensions :alexandria)
  (:shadow :set)
  (:export
   :lens
   :set :view :∘))
(in-package :optics)

;; TODO: van-laarhoven lenses??
(defstruct lens
  getter
  setter)

(defun set (lens obj &optional val)
  (funcall (lens-setter lens) obj val))

(defun view (lens obj &optional val)
  (funcall (lens-getter lens) obj val))

(defun ∘ (lens1 lens2)
  (make-lens :getter (compose (lens-getter lens1) (lens-getter lens2))
             :setter (compose (lens-setter lens1) (lens-setter lens2))))
