(in-package #:devops-helper)

;;; Configuration parameters
(defparameter *organization* nil)
(defparameter *pat* nil)
(defparameter *user-email* nil)
(defparameter *api-version* "7.1")

(defun init-config ()
  "Read environment variables from config.env at runtime."
  (cl-env:init (merge-pathnames (uiop:pathname-directory-pathname sb-ext:*core-pathname*) "config.env"))
  (setf *organization* (cl-env:getenv "DEVOPS_ORGANIZATION")
        *pat*          (cl-env:getenv "DEVOPS_USER_PAT")
        *user-email*   (cl-env:getenv "DEVOPS_USER_EMAIL")))
