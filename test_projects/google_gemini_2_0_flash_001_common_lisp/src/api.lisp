(defpackage :complang-crud.api
  (:use :cl)
  (:require :hunchentoot :cl-json :complang-crud.db :complang-crud.models)
  (:import-from :complang-crud.db
                #:get-all-users
                #:get-user-by-id
                #:create-user
                #:update-user
                #:delete-user)
  (:export #:start-api))

(in-package :complang-crud.api)

(defun send-json (data &key (status 200))
  (setf (hunchentoot:content-type*) "application/json")
  (setf (hunchentoot:return-code*) status)
  (cl-json:encode data))

(defun parse-json-params (request)
  (cl-json:decode-json-from-string (hunchentoot:raw-post-data :request request)))

(defun handle-get-users ()
  (send-json (mapcar (lambda (user)
                      (list :id (complang-crud.models:user-id user)
                            :name (complang-crud.models:user-name user)
                            :email (complang-crud.models:user-email user)))
                    (complang-crud.db:get-all-users))))

(defun handle-get-user (id)
  (let ((user (complang-crud.db:get-user-by-id id)))
    (if user
        (send-json (list :id (complang-crud.models:user-id user)
                           :name (complang-crud.models:user-name user)
                           :email (complang-crud.models:user-email user)))
        (setf (hunchentoot:return-code*) 404))))

(defun handle-post-users (request)
  (let ((params (parse-json-params request))
        (name (getf params :name))
        (email (getf params :email)))
    (if (and name email)
        (let ((user (complang-crud.db:create-user name email)))
          (send-json (list :id (complang-crud.models:user-id user)
                             :name (complang-crud.models:user-name user)
                             :email (complang-crud.models:user-email user))
                     :status 201))
        (setf (hunchentoot:return-code*) 400))))

(defun handle-put-user (id request)
  (let ((params (parse-json-params request))
        (name (getf params :name))
        (email (getf params :email)))
    (let ((updated (complang-crud.db:update-user id name email)))
      (if (> updated 0)
          (setf (hunchentoot:return-code*) 204)
          (setf (hunchentoot:return-code*) 404)))))

(defun handle-delete-user (id)
  (let ((deleted (complang-crud.db:delete-user id)))
    (if (> deleted 0)
        (setf (hunchentoot:return-code*) 204)
        (setf (hunchentoot:return-code*) 404))))

(defun dispatch-request (request)
  (let ((path (hunchentoot:request-uri* request))
        (method (hunchentoot:request-method* request)))
    (cond
      ((and (string= path "/users") (eq method :GET))
       (handle-get-users))
      ((and (string= path "/users") (eq method :POST))
       (handle-post-users request))
      ((and (>= (length path) 7) (string= (subseq path 0 6) "/users/") (eq method :GET))
       (let ((id (parse-integer (subseq path 7) :junk-allowed t)))
         (if id
             (handle-get-user id)
             (setf (hunchentoot:return-code*) 400))))
      ((and (>= (length path) 7) (string= (subseq path 0 6) "/users/") (eq method :PUT))
       (let ((id (parse-integer (subseq path 7) :junk-allowed t)))
         (if id
             (handle-put-user id request)
             (setf (hunchentoot:return-code*) 400))))
      ((and (>= (length path) 7) (string= (subseq path 0 6) "/users/") (eq method :DELETE))
       (let ((id (parse-integer (subseq path 7) :junk-allowed t)))
         (if id
             (handle-delete-user id)
             (setf (hunchentoot:return-code*) 400))))
      (t (setf (hunchentoot:return-code*) 404)))))

(defun start-api ()
  (let ((acceptor (make-instance 'hunchentoot:easy-acceptor :port 8080)))
    (hunchentoot:define-easy-handler (users-handler :uri-prefix "/") (request)
      (dispatch-request request))
    (hunchentoot:start acceptor)
    (format t "Server started on port 8080~%" )))
