(in-package #:devops-helper)

(defun get-work-item-refs ()
  "run the wiql query and return the list of work item references."
  (let* ((response (azure-wiql-query (format nil *wiql-query-all-work-items* *user-email*)))
         (json (cl-json:decode-json-from-string response)))
    (cdr (assoc :work-items json))))

(defun extract-ids (refs)
  "extract the list of ids from the work item references."
  (mapcar (lambda (ref) (assoc :id ref)) refs))

(defun azure-get-work-items (ids)
  "Given a list of work item ids, fetch their details from Azure DevOps.
If the HTTP status code is not 200 (e.g. 403), the function logs the error
(including the function name) and returns NIL."
  (let* ((function-name "azure-get-work-items")
         (id-string (format nil "~{~a~^,~}" (mapcar #'cdr ids)))
         (url (format nil "https://dev.azure.com/~a/_apis/wit/workitems?ids=~a&api-version=~a"
                      *organization* id-string *api-version*)))
    (handler-case
        (multiple-value-bind (body-or-stream status-code headers uri stream must-close reason-phrase)
            (drakma:http-request url
                                 :basic-authorization `(, *user-email* ,*pat*))
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
  (let ((sanitized-title (strip-chars title ":;'`?!'")))
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
