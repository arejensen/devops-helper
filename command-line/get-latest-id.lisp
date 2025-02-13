(in-package #:devops-helper)

(defun get-latest-id/options ()
  "Returns the options for the `get-latest-id' command"
  (list
   (clingon:make-option
    :integer
    :description "Number of work task items' ids to retrieve"
    :short-name #\n
    :long-name "number-of"
    :initial-value 1
    :key :get-latest-n)))

(defun get-latest-id/handler (cmd)
  "Handler for the `get-latest-id' command"
  (let ((number-of-items (clingon:getopt cmd :get-latest-n)))
    (format t "~{~A~^~%~}" (get-n-latest-id number-of-items))))

(defun get-latest-id/command ()
  "A command to get the latest work items you've added or updated"
  (clingon:make-command
   :name "get-latest-id"
   :description "Gets the ids of the latest work items you have added or updated"
   :options (get-latest-id/options)
   :handler #'get-latest-id/handler))
