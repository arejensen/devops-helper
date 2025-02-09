(in-package #:devops-helper)

(defun bump-tag/options ()
  "Returns the options for the `bump-tag' command"
  (list
   (clingon:make-option
    :string
    :description "Regex to match tags on"
    :short-name #\r
    :long-name "regex"
    :initial-value ".*"
    :key :regex)
   (clingon:make-option
    :string
    :description "Path to repo"
    :short-name #\p
    :long-name "path"
    :key :repo-path)))

(defun bump-tag/handler (cmd)
  "Handler for the `bump-tag' command"
  (let ((regex (clingon:getopt cmd :regex))
        (path (clingon:getopt cmd :repo-path)))
    (bump-tag regex path)))

(defun bump-tag/command ()
  "A command to bump the tag of a git repo"
  (clingon:make-command
   :name "bump-tag"
   :description "Bump the version of tag matching regex"
   :options (bump-tag/options)
   :handler #'bump-tag/handler))
