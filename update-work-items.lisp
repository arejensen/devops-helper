(defun update-work-item-state (id state)
  "Updates worktask with ID to state STATE."
  (let ((url (format nil "https://dev.azure.com/~A/~A/_apis/wit/workitems/~A?api-version=~A"
                     *organization* *organization* id *api-version*))
        (patch-content (print (cl-json:encode-json-to-string `(((op . "add")
                                                                (path . "/fields/System.State")
                                                                (value . ,state)))))))
    (drakma:http-request url
                         :basic-authorization `(,*user-email* ,*pat*)
                         :method :patch
                         :content patch-content
                         :content-type "application/json-patch+json")))
