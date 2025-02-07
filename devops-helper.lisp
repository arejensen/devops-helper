;;;; devops-helper.lisp
(in-package #:devops-helper)

(defun main ()
  "Main function to run the query and print results."
  (format t "~{~A~^~%~}" (get-n-latest 1)))
