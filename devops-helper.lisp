;;;; devops-helper.lisp
(in-package #:devops-helper)

(defun top-level/sub-commands ()
  "All the commands related to work items"
  (list
   (suggest-branch-names/command)
   (get-latest-id/command)
   (set-state/command)
   (bump-tag/command)
   (open-pullrequest/command)))

(defun top-level/options ()
  "Returns the options for the top-level command"
  nil)

(defun top-level/handler (cmd)
  "The handler for the top-level command. Will print the usage of the app"
  (clingon:print-usage-and-exit cmd t))

(defun top-level/command ()
  "Top level command for devops-helper"
  (clingon:make-command :name "devops-helper"
                        :version "0.1.0"
                        :description "Helps me manage my devops work items"
                        :long-description (format nil "Dealing with the Devops web-interface ~
                                                       is cumbersome, slow, and takes me out ~
                                                       of the flow. ~
                                                       This tool helps prevent that.")
                        :authors '("Are Jensen <arejensen@gmail.com>")
                        :license "MIT"
                        :handler #'top-level/handler
                        :options (top-level/options)
                        :sub-commands (top-level/sub-commands)))

(defun main ()
  "Entrypoint of devops-helper"
  (init-config)
  (let ((app (top-level/command)))
    (clingon:run app)))
