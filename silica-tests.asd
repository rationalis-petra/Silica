(require 'asdf)

(defsystem :silica-tests
  :name "silica-tests"
  :depends-on (:silica :parachute)
  :pathname "test"
  :components
  ((:module "typecheck"
     :pathname "typecheck"
     :depends-on ("equality")
     :components
     ((:file "environment")
      (:file "typecheck")))
   (:file "equality"  :depends-on ("silica-tests"))
   (:file "parse"     :depends-on ("silica-tests"))
   (:file "silica-tests")))
