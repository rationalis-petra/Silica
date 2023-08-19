(require 'asdf)

(defsystem :opal-tests
  :name "opal-tests"
  :depends-on (:opal :parachute)
  :pathname "test"
  :components
  ((:file "typecheck" :depends-on ("equality"))
   (:file "equality"  :depends-on ("opal-tests"))
   (:file "parse"     :depends-on ("opal-tests"))
   (:file "opal-tests")))
