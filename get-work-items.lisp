(in-package #:devops-helper)

(defun get-work-item-refs ()
  "run the wiql query and return the list of work item references."
  (let* ((response (wiql-query (format nil *wiql-query-all-work-items* *user-email*)))
         (json (cl-json:decode-json-from-string response)))
    (cdr (assoc :work-items json))))

(defun extract-ids (refs)
  "extract the list of ids from the work item references."
  (mapcar (lambda (ref) (assoc :id ref)) refs))

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

(defun extract-work-items-ids (work-items)
  "Return all work items' ids."
  (let ((work-items (cdr (assoc :VALUE work-items))))
    (mapcar #'list (mapcar #'extract-work-item-id work-items))))

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

(defun format-work-item-id (work-item)
  "Formats a single work-item's id (an association list) into a string.
WORK-ITEM is expected to have associations for 'ID"
  (cdr (assoc :ID work-item)))

(defun format-branch-names (work-items)
  "Given a list of work-items, returns a list of formatted strings."
  (mapcar #'format-work-item work-items))

(defun format-work-items-ids (work-items)
  "Given a list of work-items, returns a list of work item ids."
  (mapcar #'format-work-item-id work-items))

(defun suggest-n-branch-names (n)
  "Query Azure DevOps for work items and print out suitable branch name suggestions."
  (let* ((refs (get-work-item-refs))
         (ids (extract-ids refs)))
    (let ((work-items (azure-get-work-items (subseq ids 0 n))))
      (format-branch-names (extract-work-items work-items)))))

(defun get-n-latest-id (n)
  "Query Azure DevOps for work items and print out their ids."
  (let* ((refs (get-work-item-refs))
         (ids (extract-ids refs)))
    (let ((work-items (azure-get-work-items (subseq ids 0 n))))
      (format-work-items-ids (extract-work-items-ids work-items)))))
