(in-package #:devops-helper)

(defun set-work-item-state (id state)
  "Set worktask with ID to state STATE."
  (let ((function-name "set-work-item-state")
        (url (format nil "https://dev.azure.com/~A/~A/_apis/wit/workitems/~A?api-version=~A"
                     *organization* *organization* id *api-version*))
        (patch-content (cl-json:encode-json-to-string `(((op . "add")
                                                         (path . "/fields/System.State")
                                                         (value . ,state))))))
    (handler-case
        (multiple-value-bind (body-or-stream status-code headers uri stream must-close reason-phrase)
            (drakma:http-request url
                                 :basic-authorization `(,*user-email* ,*pat*)
                                 :method :patch
                                 :content patch-content
                                 :content-type "application/json-patch+json")

          (declare (ignore headers uri stream must-close))
          (if (not (eql status-code 200))
              (progn
                (format t "HTTP error in ~A: status code ~A, reason: ~A~%"
                        function-name status-code reason-phrase)
                nil)
              (cl-json:decode-json-from-string
               (flexi-streams:octets-to-string body-or-stream))))
      (drakma:drakma-error (e)
        (format t "Drakma error in ~A: ~A~%" function-name e)
        nil)
      (error (e)
        (format t "Unexpected error in ~A: ~A~%" function-name e)
        nil))))
