(in-package #:devops-helper)

(defparameter *launcher*
  #+(or win32 mswindows windows)
  "start ~S"
  #+(or macos darwin)
  "open ~S"
  #-(or win32 mswindows macos darwin windows)
  "xdg-open ~S")

(defun open-url-in-browser (url)
  (uiop:run-program (format nil *launcher* url)))
