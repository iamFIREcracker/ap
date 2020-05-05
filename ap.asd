(asdf:defsystem #:ap
  :description "Matteo's resource planner"
  :author "Matteo Landi <matteo@matteolandi.net>"
  :license  "MIT"
  :version "0.0.3"
  :depends-on (
               #:pileup
               #:split-sequence
               #:unix-opts
               )

  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "ap"
  :entry-point "ap:toplevel"

  :in-order-to ((test-op (test-op :ap/tests)))
  :serial t
  :components
  ((:module "vendor"
    :serial t
    :components ((:file "pmdb")
                 (:file "quickutils-package")
                 (:file "quickutils")
                 (:file "utils-package")
                 (:file "utils")))
   (:file "package")
   (:module "src" :serial t
            :components
            ((:file "main")))))

(asdf:defsystem :ap/tests
  :description "Matteo's resource planner"
  :author "Matteo Landi <matteo@matteolandi.net>"
  :license  "MIT"
  :version "0.0.3"
  :depends-on (
               #:ap
               #:1am
               )
  :serial t
  :components
  ((:module "test"
    :serial t
    :components ((:file "main"))))
  :perform (test-op (o c) (uiop:symbol-call :1am '#:run)))
