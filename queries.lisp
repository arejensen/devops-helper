(in-package #:devops-helper)

(defparameter *wiql-query-all-work-items*
  "SELECT [System.Id]
               FROM WorkItems
               WHERE [System.WorkItemType] IN ('Task', 'Bug', 'Product Backlog Item')
                   AND [System.ChangedBy] = '~A'
                   AND [System.State] != 'Removed'
               ORDER BY [System.ChangedDate] DESC"
  "WIQL (Work Item Query Language) query. WIQL is a sql-ish query language for querying work items in devops.")

(defun azure-wiql-query (wiql)
  "Send the WIQL query to Azure DevOps and return the response as a string.
If the HTTP status code is not 200 (e.g. 403), the function logs the error
(including the file and function name) and returns NIL."
  (let* ((function-name "azure-wiql-query")
         (url (format nil "https://dev.azure.com/~A/_apis/wit/wiql?api-version=~A"
                      *organization* *api-version*))
         (json-body (cl-json:encode-json-to-string `(("query" . ,wiql)))))
    (handler-case
        (multiple-value-bind (body-or-stream status-code headers uri stream must-close reason-phrase)
            (drakma:http-request url
                                 :method :post
                                 :basic-authorization `(, *user-email* , *pat*)
                                 :content-type "application/json"
                                 :content json-body)
          (declare (ignore headers uri stream must-close))
          (if (not (eql status-code 200))
              (progn
                (format t "HTTP error in ~A: status code ~A, reason: ~A~%"
                        function-name status-code reason-phrase)
                nil)
              (flexi-streams:octets-to-string body-or-stream)))
      (drakma:drakma-error (e)
        (format t "Drakma error in ~A: ~A~%"
                function-name e)
        nil)
      (error (e)
        (format t "Unexpected error in ~A: ~A~%"
                function-name e)
        nil))))
