(defpackage silica
  (:use :cl :trivia :iter :alexandria :lang-extensions)
  (:shadow :set)
  (:export
   ;; get silica values from CL
   :run-main
   :get-silica-val

   :*silica-modules*

   ;; Readtables
   :classic

   ;; pipeline functions
   :infixify :to-ast :to-def  ;; parse
   :check :infer ;; typecheck
   :reify ;; compile

   ;; comparisons
   :α= :α>= :α<=
   :β= :β<= :β>=

   ;; Constructors and values
   :mk-var :mk-app :mk-λ :mk-arr :mk-entry
   :mk-mλ :mk-struct :mk-abs :mk-val :mk-proj :mk-mvar
   :mk-tλ :mk-∀ :mk-sig :mk-native :mk-tapp :mk-tvar :mk-induct
   :mk-kind :mk-karr
   :mk-def :mk-decl
   :kind-type
   :+empty-env+))
