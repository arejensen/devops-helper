(in-package #:devops-helper)

(defun open-pullrequest/handler (cmd)
  "Handler for the `open-pullrequest' command"
  (declare (ignore cmd))
  (trivial-open-browser:open-browser
      (format nil "~A/pullrequestcreate?sourceRef=~A"
              (with-output-to-string (s)
                  (let ((*standard-output* s))
                      (legit:git-config :name "remote.origin.url")))
              (with-output-to-string (s)
                  (let ((*standard-output* s))
                      (legit:git-rev-parse "HEAD" :abbrev-ref :strict))))))

(defun open-pullrequest/command ()
  "A command to open a pull request of the current branch"
  (clingon:make-command
   :name "open-pullrequest"
   :description "Open a pull request of the current branch into the default branch"
   :handler #'open-pullrequest/handler))
