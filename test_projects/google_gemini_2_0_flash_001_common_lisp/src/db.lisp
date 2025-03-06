(defpackage :complang-crud.db
  (:use :cl)
  (:require :cl-postgres)
  (:import-from :complang-crud.models
                #:user
                #:make-user
                #:user-id
                #:user-name
                #:user-email)
  (:export #:connect-db
           #:get-all-users
           #:get-user-by-id
           #:create-user
           #:update-user
           #:delete-user))

(in-package :complang-crud.db)

(defparameter *db-connection* nil)

(defun connect-db ()
  (setf *db-connection*
        (cl-postgres:connect
         :host "host.docker.internal"
         :port 5432
         :database "complang"
         :user "testuser"
         :password (uiop:getenv "PGPASSWORD"))))

(defun get-all-users ()
  (connect-db)
  (cl-postgres:exec-query *db-connection* "SELECT id, name, email FROM users")
  (let ((result (cl-postgres:get-query-result *db-connection*)))
    (mapcar (lambda (row)
              (make-user :id (parse-integer (getf row :|id|))
                         :name (getf row :|name|)
                         :email (getf row :|email|)))
            (cl-postgres:fetch-results result))))

(defun get-user-by-id (id)
  (connect-db)
  (cl-postgres:exec-query *db-connection* "SELECT id, name, email FROM users WHERE id = $1" (list id))
  (let ((result (cl-postgres:get-query-result *db-connection*)))
    (let ((row (first (cl-postgres:fetch-results result))))
      (if row
          (make-user :id (parse-integer (getf row :|id|))
                     :name (getf row :|name|)
                     :email (getf row :|email|))
          nil))))

(defun create-user (name email)
  (connect-db)
  (cl-postgres:exec-query *db-connection*
                           "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email"
                           (list name email))
  (let ((result (cl-postgres:get-query-result *db-connection*)))
    (let ((row (first (cl-postgres:fetch-results result))))
      (make-user :id (parse-integer (getf row :|id|))
                 :name (getf row :|name|)
                 :email (getf row :|email|)))))

(defun update-user (id name email)
  (connect-db)
  (cl-postgres:exec-query *db-connection*
                           "UPDATE users SET name = $2, email = $3 WHERE id = $1"
                           (list id name email))
  (cl-postgres:affected-rows (cl-postgres:get-query-result *db-connection*)))

(defun delete-user (id)
  (connect-db)
  (cl-postgres:exec-query *db-connection* "DELETE FROM users WHERE id = $1" (list id))
  (cl-postgres:affected-rows (cl-postgres:get-query-result *db-connection*)))
