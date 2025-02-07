(in-package #:devops-helper)

;;; Configuration parameters
(cl-env:init "config.env")
(defparameter *organization* (cl-env:getenv "DEVOPS_ORGANIZATION"))
(defparameter *pat* (cl-env:getenv "DEVOPS_USER_PAT"))
(defparameter *user-email* (cl-env:getenv "DEVOPS_USER_EMAIL"))
(defparameter *api-version* "7.1")
