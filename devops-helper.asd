;;;; devops-helper.asd
(push "~/quicklisp/local-projects/devops-helper/" asdf:*central-registry*)
(asdf:defsystem #:devops-helper
  :description "Helper for interacting with devops work items"
  :author "Are Jensen"
  :license  "MIT"
  :version "0.0.1"
  :depends-on (:drakma :cl-json :cl-env :clingon :legit)
  :serial t
  :build-operation "program-op"
  :build-pathname "devops-helper"
  :entry-point "devops-helper:main"
  :components ((:file "package")
               (:file "open-browser")
               (:file "environment-variables")
               (:file "azure-calls")
               (:file "queries")
               (:file "get-work-items")
               (:file "bump-tag")
               (:file "command-line/suggest-branch-names")
               (:file "command-line/get-latest-id")
               (:file "command-line/set-state")
               (:file "command-line/bump-tag")
               (:file "command-line/open-pullrequest")
               (:file "devops-helper")))
