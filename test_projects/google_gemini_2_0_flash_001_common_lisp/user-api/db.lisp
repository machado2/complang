
(ql:quickload :cl-postgres)

(defpackage :user-api.db
  (:use :cl)
  (:export :connect-db
           :get-all-users
           :get-user-by-id
           :create-user
           :update-user
           :delete-user))

(in-package :user-api.db)

(defun connect-db ()
  (cl-postgres:connect
   :database-name "complang"
   :username "testuser"
   :password (or (uiop:getenv "PGPASSWORD") "Saloon5-Moody-Observing")
   :host "host.docker.internal"
   :port 5432))

(defun get-all-users ()
  (let ((conn (connect-db)))
    (cl-postgres:exec-query conn "SELECT id, name, email FROM users")
    (mapcar (lambda (row)
              (list :id (getf row :|id|) :name (getf row :|name|) :email (getf row :|email|)))
            (cl-postgres:fetch-rows conn))))

(defun get-user-by-id (id)
  (let ((conn (connect-db)))
    (cl-postgres:exec-query conn "SELECT id, name, email FROM users WHERE id = $1" (list id))
    (let ((rows (cl-postgres:fetch-rows conn)))
      (if rows
          (let ((row (car rows)))
            (list :id (getf row :|id|) :name (getf row :|name|) :email (getf row :|email|})))
          nil))))

(defun create-user (name email)
  (let ((conn (connect-db)))
    (cl-postgres:exec-query conn "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id" (list name email))
    (let ((rows (cl-postgres:fetch-rows conn)))
      (if rows
          (let ((row (car rows)))
            (getf row :|id|))
          nil))))

(defun update-user (id name email)
  (let ((conn (connect-db)))
    (cl-postgres:exec-query conn "UPDATE users SET name = $2, email = $3 WHERE id = $1" (list id name email))
    (cl-postgres:affected-rows conn)))

(defun delete-user (id)
  (let ((conn (connect-db)))
    (cl-postgres:exec-query conn "DELETE FROM users WHERE id = $1" (list id))
    (cl-postgres:affected-rows conn))))
