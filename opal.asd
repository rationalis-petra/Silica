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
               :esrap)
  :pathname "src"
  :components
  ((:file "opal" :depends-on ("package"))
   (:file "package")))
