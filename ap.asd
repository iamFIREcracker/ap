(asdf:defsystem #:ap
  :description "Matteo's resource planner"
  :author "Matteo Landi <matteo@matteolandi.net>"
  :license  "MIT"
  :version "0.0.0"
  :depends-on (
               #:pileup
               #:split-sequence
               #:unix-opts
               )

  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "ap"
  :entry-point "ap:toplevel"

  :serial t
  :components
  ((:module "vendor"
    :serial t
    :components ((:file "pmdb")
                 (:file "quickutils")
                 (:file "utils")))
   (:file "package")
   (:module "src" :serial t
            :components
            ((:file "main")))))

(asdf:defsystem :ap/tests
  :description "Matteo's resource planner"
  :author "Matteo Landi <matteo@matteolandi.net>"
  :license  "MIT"
  :version "0.0.0"
  :depends-on (
               #:ap
               #:1am
               )
  :perform (test-op (o c) (uiop:symbol-call :1am '#:run)))
