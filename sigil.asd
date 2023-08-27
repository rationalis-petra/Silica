(require 'asdf)

(defsystem :sigil
  :name "sigil"
  :version "1.0.0"
  :maintainer "Connor Redfern"
  :license "BSD-3"
  :description "Sigil: a functional langauge implemented on top of lisp."
  :depends-on (:asdf
               :alexandria
               :iterate
               :trivia
               :cl-unicode
               :named-readtables)
  :pathname "src"
  :components
  ((:file "embed" :depends-on ("module"))
   (:file "module" :depends-on ("parse" "codegen" "typecheck"))
   (:module "codegen"
     :pathname "codegen"
     :depends-on ("syntax")
     :components
     ((:file "codegen" :depends-on ("context" "representation"))
      (:file "representation")
      (:file "context")))
   (:module "parse"
     :pathname "parse"
     :depends-on ("syntax")
     :components
     ((:file "parse")
      (:file "reader" :depends-on ("parse"))))
   (:module "typecheck"
     :pathname "typecheck"
     :depends-on ("syntax")
     :components
     ((:file "typecheck" :depends-on ("type-manipulation"))
      (:file "type-manipulation")))
   (:file "syntax" :depends-on ("sigil"))
   (:file "sigil" :depends-on ("containers" "lang"))

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

