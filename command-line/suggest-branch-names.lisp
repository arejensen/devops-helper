(in-package #:devops-helper)

(defun suggest-branch-names/options ()
  "Returns the options for the `suggest-branch-name' command"
  (list
   (clingon:make-option
    :integer
    :description "Number of branch suggestions to retrieve"
    :short-name #\n
    :long-name "number-of"
    :initial-value 1
    :key :suggest-n-branch-names)))

(defun suggest-branch-names/handler (cmd)
  "Handler for the `suggest-branch-name' command"
  (let ((number-of-items (clingon:getopt cmd :suggest-n-branch-names)))
    (format t "~{~A~^~%~}" (suggest-n-branch-names number-of-items))))

(defun suggest-branch-names/command ()
  "A command to get the latest work items you've added or updated"
  (clingon:make-command
   :name "suggest-branchname"
   :description "Suggest branchnames for the latest work items you have added or updated"
   :options (suggest-branch-names/options)
   :handler #'suggest-branch-names/handler))
