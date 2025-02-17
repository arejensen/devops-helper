(in-package #:devops-helper)

(defun open-pullrequest/handler (cmd)
  "Handler for the `open-pullrequest' command")
    ;;; TODO: trivial-open-browser looks promising for this

(defun open-pullrequest/command ()
  "A command to open a pull request of the current branch"
  (clingon:make-command
   :name "open-pullrequest"
   :description "Open a pull request of the current branch into the default branch"
   :handler #'open-pullrequest/handler))
