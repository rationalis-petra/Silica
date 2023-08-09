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
               :cl-unicode
               :named-readtables)
  :pathname "src"
  :components
  ((:file "embed" :depends-on ("parse" "codegen"))

   (:file "codegen" :depends-on ("syntax"))
   (:file "parse" :depends-on ("syntax"))
   (:file "typecheck" :depends-on ("syntax"))
   (:file "syntax" :depends-on ("opal"))
   (:file "opal")))

(defsystem :opal.tests
  :name "opal tests"
  :depends-on (:opal :parachute)
  :pathname "test"
  :components
  ((:file "typecheck" :depends-on ("opal-tests"))
   (:file "equality" :depends-on ("opal-tests"))
   (:file "parse" :depends-on ("opal-tests"))
   (:file "opal-tests")))
