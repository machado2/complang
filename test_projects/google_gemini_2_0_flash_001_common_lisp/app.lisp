
(load "/root/quicklisp/setup.lisp")
(ql:quickload :clack)

(defpackage :simple-server
  (:use :cl :clack))

(in-package :simple-server)

(defun handle-request (request)
  (declare (ignore request))
  '(200
    (:content-type "text/plain")
    ("Hello, world!")))

(defun main ()
  (let ((port (parse-integer (or (uiop:getenv "PORT") "8080"))))
    (format t "Starting server on port ~a~%" port)
    (clack:clackup #'handle-request :port port)))

(main)
