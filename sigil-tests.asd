(require 'asdf)

(defsystem :sigil-tests
  :name "sigil-tests"
  :depends-on (:sigil :parachute)
  :pathname "test"
  :components
  ((:file "typecheck" :depends-on ("equality"))
   (:file "equality"  :depends-on ("sigil-tests"))
   (:file "parse"     :depends-on ("sigil-tests"))
   (:file "sigil-tests")))
