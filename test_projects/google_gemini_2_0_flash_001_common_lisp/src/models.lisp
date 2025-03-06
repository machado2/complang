(defpackage :complang-crud.models
  (:use :cl)
  (:export #:user
           #:make-user
           #:user-id
           #:user-name
           #:user-email))

(in-package :complang-crud.models)

(defclass user ()
  ((id
    :initarg :id
    :accessor user-id
    :initform nil)
   (name
    :initarg :name
    :accessor user-name
    :initform nil)
   (email
    :initarg :email
    :accessor user-email
    :initform nil)))

(defun make-user (&key id name email)
  (make-instance 'user :id id :name name :email email))
