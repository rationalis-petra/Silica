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
  ((:file "embed" :depends-on ("parse" "codegen" "typecheck"))
   (:file "codegen" :depends-on ("syntax"))
   (:module "parse"
    :pathname "parse"
    :depends-on ("syntax")
    :components
    ((:file "parse")))
   (:module "typecheck"
    :pathname "typecheck"
    :depends-on ("syntax")
    :components
    ((:file "typecheck" :depends-on ("type-manipulation"))
     (:file "type-manipulation")))
   (:file "syntax" :depends-on ("opal"))
   (:file "opal" :depends-on ("containers" "lang"))

   (:module "lang"
    :pathname "utils"
    :components
    ((:file "language")))

   (:module "containers"
    :pathname "utils/containers"
    :components
     ((:file "array")
      (:file "hash-table")
      (:file "alist")
      (:file "list")))))

(defsystem :opal.tests
  :name "opal tests"
  :depends-on (:opal :parachute)
  :pathname "test"
  :components
  ((:file "typecheck" :depends-on ("opal-tests"))
   (:file "equality" :depends-on ("opal-tests"))
   (:file "parse" :depends-on ("opal-tests"))
   (:file "opal-tests")))
