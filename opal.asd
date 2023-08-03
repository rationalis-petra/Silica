(require 'asdf)

(defsystem :opal
  :name "opal"
  :version "1.0.0"
  :maintainer "Connor Redfern"
  :license "BSD-3"
  :description "Opal: a functional langauge implemented on top of lisp."
  :depends-on (:asdf
               :alexandria
               :iterate
               :trivia
               :esrap
               :named-readtables)
  :pathname "src"
  :components
  ((:file "embed" :depends-on ("parse" "codegen"))

   (:file "codegen" :depends-on ("syntax"))
   (:file "parse" :depends-on ("syntax"))
   (:file "typecheck" :depends-on ("syntax"))
   (:file "syntax" :depends-on ("opal"))
   (:file "opal")))
