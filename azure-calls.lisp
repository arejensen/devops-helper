(in-package #:devops-helper)

(defmacro define-azure (fn-name params docstring &key (method :get) (decode-json? t) (content nil) (content-type "application/json") url)
  (let ((method-gensym         (gensym "METHOD-"))
        (decode-json?-gensym   (gensym "DECODE-JSON-"))
        (content-gensym        (gensym "CONTENT-"))
        (content-type-gensym   (gensym "CONTENT-TYPE-"))
        (url-gensym            (gensym "URL-"))
        (function-name-gensym  (gensym "FUNC-NAME-")))
    `(defun ,fn-name ,params
       ,docstring
       (let ((,method-gensym       ,method)
             (,decode-json?-gensym ,decode-json?)
             (,content-gensym      ,content)
             (,content-type-gensym ,content-type)
             (,url-gensym          ,url)
             (,function-name-gensym (string ',fn-name)))
         (handler-case
             (multiple-value-bind (body-or-stream
                                   status-code
                                   headers
                                   uri
                                   stream
                                   must-close
                                   reason-phrase)
                 (drakma:http-request ,url-gensym
                                      :basic-authorization `(,*user-email* ,*pat*)
                                      :method ,method-gensym
                                      :content ,content-gensym
                                      :content-type ,content-type-gensym)
               (declare (ignore headers uri stream must-close))
               (if (not (eql status-code 200))
                   (progn
                     (format t "HTTP error in ~A: status code ~A, reason: ~A~%"
                             ,function-name-gensym status-code reason-phrase)
                     (error "Could not proceed due to error"))
                   (let ((text-response (flexi-streams:octets-to-string body-or-stream)))
                     (if ,decode-json?-gensym
                         (cl-json:decode-json-from-string text-response)
                         text-response))))
           (drakma:drakma-error (e)
             (format t "Drakma error in ~A: ~A~%" ,function-name-gensym e)
             nil)
           (error (e)
             (format t "Unexpected error in ~A: ~A~%" ,function-name-gensym e)
             nil))))))

(define-azure wiql-query (wiql)
  "Sends a WIQL query to Azure DevOps and returns the result as raw text or NIL on error."
  :method :post
  :decode-json? nil
  :content-type "application/json"
  :url (format nil "https://dev.azure.com/~A/_apis/wit/wiql?api-version=~A"
               *organization*
               *api-version*)
  :content (cl-json:encode-json-to-string
            `(("query" . ,wiql))))

(define-azure set-work-item-state (id state)
  "Set the work item with ID to the given STATE."
  :method :patch
  :content-type "application/json-patch+json"
  :url (format nil "https://dev.azure.com/~a/~a/_apis/wit/workitems/~a?api-version=~a"
               *organization*
               *organization*
               id
               *api-version*)
  :content (cl-json:encode-json-to-string
            `(((op . "add")
               (path . "/fields/System.State")
               (value . ,state)))))

(define-azure azure-get-work-items (ids)
  "Fetch details for a list of work item IDs from Azure DevOps, returning JSON on success."
  :method :get
  :url (let ((id-string (format nil "~{~a~^,~}" (mapcar #'cdr ids))))
         (format nil "https://dev.azure.com/~a/_apis/wit/workitems?ids=~a&api-version=~a"
                 *organization* id-string *api-version*)))
