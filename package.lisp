;;;; package.lisp

(defpackage #:devops-helper
  (:use :cl)
  (:export :main))

(ql:quickload 'drakma)
(ql:quickload 'cl-json)
(ql:quickload 'cl-env)
(ql:quickload 'cl-ppcre)
(ql:quickload 'legit)
