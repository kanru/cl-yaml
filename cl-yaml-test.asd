(asdf:defsystem cl-yaml-test
  :version "0.1.0"
  :author "Kan-Ru Chen (陳侃如) <kanru@kanru.info>"
  :licence "MIT/Expat"
  :components ((:module "t"
                :components
                ((:module "base"
                  :pathname ""
                  :components ((:file "test")))
                 (:module "tests"
                  :pathname ""
                  :depends-on ("base")
                  :components ((:file "test-reader"))))))
  :depends-on ("cl-yaml"
               "cl-test-more"))
