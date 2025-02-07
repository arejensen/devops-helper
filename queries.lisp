(in-package #:devops-helper)

(defparameter *wiql-query-all-work-items*
  (format nil "SELECT [System.Id]
               FROM WorkItems
               WHERE [System.WorkItemType] IN ('Task', 'Bug', 'Product Backlog Item')
                   AND [System.ChangedBy] = '~A'
                   AND [System.State] != 'Removed'
               ORDER BY [System.ChangedDate] DESC"
          *user-email*)
  "WIQL (Work Item Query Language) query. WIQL is a sql-ish query language for querying work items in devops.")

(defun azure-wiql-query (wiql)
  "Send the WIQL query to Azure DevOps and return the response as a string."
  (let* ((url (format nil "https://dev.azure.com/~A/_apis/wit/wiql?api-version=~A"
                      *organization* *api-version*))
         (json-body (cl-json:encode-json-to-string `(("query" . ,wiql)))))
    (flexi-streams:octets-to-string
     (drakma:http-request url
                          :method :post
                          :basic-authorization `(,*user-email* ,*pat*)
                          :content-type "application/json"
                          :content json-body))))
