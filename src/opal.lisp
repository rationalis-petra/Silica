(defpackage opal
  (:use :cl :trivia :iter)
  (:export
   ;; get opal values from CL
   :run-main
   :get-opal-val

   :*opal-modules*

   ;; Readtables
   :classic

   ;; pipeline functions
   :infixify :to-ast :to-def  ;; parse
   :check :infer ;; typecheck
   :reify ;; compile

   ;; comparisons
   :α=

   ;; Constructors and values
   :mk-λ :mk-struct :mk-var :mk-abs :mk-val :mk-app :mk-proj
   :mk-∀ :mk-sig :mk-arr :mk-native :mk-tapp :mk-tvar :mk-mvar
   :mk-kind :mk-karr
   :mk-def :mk-decl
   :kind-type
   :+empty-env+))
