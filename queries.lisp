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

