(defpackage opal
  (:use :cl :trivia :iter)
  (:export
   :run-main
   :*opal-modules*
   :classic
   :to-ast :to-def :reify
   :check :infer

   ;; comparions
   :α=

   ;; constructors
   :mk-λ :mk-struct :mk-var :mk-abs :mk-val
   :mk-∀ :mk-sig :mk-arr
   :mk-def :mk-decl
   :kind-type))
