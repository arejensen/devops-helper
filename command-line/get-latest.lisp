(in-package #:devops-helper)

(defun get-latest/options ()
  "Returns the options for the `get-latest' command"
  (list
   (clingon:make-option
    :integer
    :description "Number of work task items to retrieve"
    :short-name #\n
    :long-name "number-of"
    :initial-value 1
    :key :get-latest-n)))

(defun get-latest/handler (cmd)
  "Handler for the `get-latest' command"
  (let ((number-of-items (clingon:getopt cmd :get-latest-n)))
    (format t "~{~A~^~%~}" (get-n-latest number-of-items))))

(defun get-latest/command ()
  "A command to get the latest work items you've added or updated"
  (clingon:make-command
   :name "get-latest"
   :description "Gets the latest work items you have added or updated"
   :options (get-latest/options)
   :handler #'get-latest/handler))
