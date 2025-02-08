(in-package #:devops-helper)

(defun set-state/options ()
  "Returns the options for the `set-state' command"
  (list
   (clingon:make-option
    :integer
    :description "Id of the work item"
    :short-name #\i
    :long-name "id"
    :key :id)
   (clingon:make-option
    :enum
    :description "State to set the work item to"
    :short-name #\s
    :items '(("To Do" . "To Do")
             ("In Progress" . "In Progress")
             ("Review" . "Review")
             ("Done" . "Done"))
    :long-name "state"
    :key :state)))

(defun set-state/handler (cmd)
  "Handler for the `set-state' command"
  (let ((id (clingon:getopt cmd :id))
        (state (clingon:getopt cmd :state)))
    (set-work-item-state id state)))

(defun set-state/command ()
  "A command to set the state of a work item"
  (clingon:make-command
   :name "set-state"
   :description "Sets state of work item"
   :options (set-state/options)
   :handler #'set-state/handler))
