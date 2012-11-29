;;;; cl-yaml.asd

(in-package #:cl-user)

(asdf:defsystem #:cl-yaml
  :description "Common Lisp YAML Parser & Emitter"
  :author "Kan-Ru Chen <kanru@kanru.info>"
  :license "MIT/Expat"
  :serial t
  :components ((:module "src"
                :components
                ((:module "base"
                  :pathname ""
                  :components ((:file "packages")))
                 (:module "parser"
                  :pathname ""
                  :depends-on ("base")
                  :components ((:file "reader")
                               (:file "parser")))))))
