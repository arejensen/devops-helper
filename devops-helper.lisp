;;;; devops-helper.lisp
(in-package #:devops-helper)

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

(defun get-work-item-refs ()
  "Run the WIQL query and return the list of work item references."
  (let* ((response (azure-wiql-query *wiql-query-all-work-items*))
         (json (cl-json:decode-json-from-string response)))
        (cdr (assoc :WORK-ITEMS json))))

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

(defun extract-ids (refs)
  "Extract the list of IDs from the work item references."
  (mapcar (lambda (ref) (assoc :ID ref)) refs))

(defun azure-get-work-items (ids)
  "Given a list of work item IDs, fetch their details from Azure DevOps."
  (let* ((id-string (format nil "~{~A~^,~}" (mapcar #'cdr ids)))
         (url (format nil "https://dev.azure.com/~A/_apis/wit/workitems?ids=~A&api-version=~A"
                      *organization* id-string *api-version*)))
    (cl-json:decode-json-from-string
     (flexi-streams:octets-to-string
      (drakma:http-request url :basic-authorization `(,*user-email* ,*pat*))))))

(defun extract-work-item-title (work-item)
  "Return a work item's title."
  (assoc :*SYSTEM.*TITLE (cdr (assoc :FIELDS work-item))))

(defun extract-work-item-type (work-item)
  "Return a work item's type."
  (assoc :*SYSTEM.*WORK-ITEM-TYPE (cdr (assoc :FIELDS work-item))))

(defun extract-work-item-id (work-item)
  "Return a work item's id."
  (assoc :ID work-item))

(defun extract-work-items (work-items)
  "Return all work items."
  (let ((work-items (cdr (assoc :VALUE work-items))))
    (mapcar #'list (mapcar #'extract-work-item-id work-items)
                   (mapcar #'extract-work-item-title work-items)
                   (mapcar #'extract-work-item-type work-items))))

(defun strip-chars (str chars-to-strip)
  "Return a new string like STR but with all characters found in CHARS-TO-STRIP removed.
CHARS-TO-STRIP can be any sequence of characters."
  (coerce (remove-if (lambda (ch)
                       (find ch chars-to-strip :test #'char-equal))
                     str)
          'string))

(defun transform-title (title)
  "Converts TITLE to lowercase and replaces spaces with dashes."
  (let ((sanitized-title (strip-chars title ":'`?!'")))
    (with-output-to-string (out)
      (loop for char across (string-downcase sanitized-title) do
            (write-char (if (char= char #\Space) #\- char) out)))))

(defun get-prefix (work-item-type)
  "Returns the prefix string based on the work-item type.
For example, 'Product Backlog Item' yields \"pbi/\"."
  (cond ((string= work-item-type "Product Backlog Item") "pbi/")
        ((string= work-item-type "Bug") "bug/")
        ((string= work-item-type "Task") "task/")
        (t (concatenate 'string (string-downcase work-item-type) "/"))))

(defun format-work-item (work-item)
  "Formats a single work-item (an association list) into a string.
WORK-ITEM is expected to have associations for 'ID, '*SYSTEM.*TITLE,
and '*SYSTEM.*WORK-ITEM-TYPE."
  (let* ((id           (cdr (assoc :ID work-item)))
         (title        (cdr (assoc :*SYSTEM.*TITLE work-item)))
         (item-type    (cdr (assoc :*SYSTEM.*WORK-ITEM-TYPE work-item)))
         (prefix       (get-prefix item-type))
         (formatted-title (transform-title title)))
    (format nil "~a~a-~a" prefix id formatted-title)))

(defun format-work-items (work-items)
  "Given a list of work-items, returns a list of formatted strings."
  (mapcar #'format-work-item work-items))

(defun get-n-latest (n)
  "Query Azure DevOps for work items and print out their details."
  (let* ((refs (get-work-item-refs))
         (ids (extract-ids refs)))
    (let ((work-items (azure-get-work-items (subseq ids 0 n))))
      (format-work-items (extract-work-items work-items)))))

(defun main ()
  "Main function to run the query and print results."
  (format t "~{~A~^~%~}" (get-n-latest 1)))
