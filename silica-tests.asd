(require 'asdf)

(defsystem :silica-tests
  :name "silica-tests"
  :depends-on (:silica :parachute)
  :pathname "test"
  :components
  ((:file "typecheck" :depends-on ("equality"))
   (:file "equality"  :depends-on ("silica-tests"))
   (:file "parse"     :depends-on ("silica-tests"))
   (:file "silica-tests")))
