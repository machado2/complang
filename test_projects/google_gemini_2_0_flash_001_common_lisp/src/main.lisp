(defpackage :complang-crud.main
  (:use :cl)
  (:require :complang-crud.api)
  (:export :main))

(in-package :complang-crud.main)

(defun main ()
  (complang-crud.api:start-api)
  (format t "Application started.~%!"))
