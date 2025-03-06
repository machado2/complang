; task1.lisp

(defpackage :task1
  (:use :cl :clack :cl-json :sxql :dbi :cl-postgres)
  (:export start-server))

(in-package :task1)

(defparameter *db-host* "host.docker.internal")
(defparameter *db-port* 5432)
(defparameter *db-name* "complang")
(defparameter *db-user* "testuser")
(defparameter *db-password* (or (uiop:getenv "PGPASSWORD") "Saloon5-Moody-Observing"))

(defparameter *db* nil)

(defun connect-db ()
  (setf *db* (dbi:connect :postgresql
                         :host *db-host*
                         :port *db-port*
                         :database-name *db-name*
                         :username *db-user*
                         :password *db-password*)))

(defun disconnect-db ()
  (when *db*
    (dbi:disconnect *db*)
    (setf *db* nil)))

(defun get-users ()
  (connect-db)
  (let ((results (dbi:fetch-all (dbi:query *db* "SELECT id, name, email FROM users"))))
    (disconnect-db)
    (mapcar (lambda (row) `((:id . ,(getf row :|id|))
                             (:name . ,(getf row :|name|))
                             (:email . ,(getf row :|email|)))) results)))

(defun get-user (id)
  (connect-db)
  (let ((result (dbi:fetch (dbi:query *db* (format nil "SELECT id, name, email FROM users WHERE id = ~A" id)))))
    (disconnect-db)
    (if result
        `((:id . ,(getf result :|id|))
          (:name . ,(getf result :|name|))
          (:email . ,(getf result :|email|))) 
        nil)))

(defun create-user (name email)
  (connect-db)
    (dbi:with-transaction *db*
  (dbi:do-sql (dbi:prepare *db* "INSERT INTO users (name, email) VALUES (?, ?)") name email))
     (let ((id (dbi:fetch-one (dbi:query *db* "SELECT last_value FROM users_id_seq"))))
    (disconnect-db)
          (list :id (getf id :|last_value|) :name name :email email)))

(defun update-user (id name email)
  (connect-db)
  (dbi:do-sql (dbi:prepare *db* (format nil "UPDATE users SET name = ?, email = ? WHERE id = ~A" id)) name email)
  (disconnect-db))

(defun delete-user (id)
  (connect-db)
  (dbi:do-sql (dbi:prepare *db* (format nil "DELETE FROM users WHERE id = ~A" id)))
  (disconnect-db))

(defun handle-users (params)
  (case (getf params :request-method)
    (:GET
     (if (getf params :path-info)
         (let ((id (parse-integer (getf params :path-info) :junk-allowed t)))
           (if id
               (let ((user (get-user id))) 
                 (if user
                     (list :status 200 :body (json:encode user) :headers '((content-type . "application/json")))
                     (list :status 404 :body "User not found" :headers '((content-type . "text/plain")))))
               (list :status 400 :body "Invalid user ID" :headers '((content-type . "text/plain")))))
         (let ((users (get-users)))
           (list :status 200 :body (json:encode users) :headers '((content-type . "application/json"))))))
    (:POST
     (let* ((body (flex:flex-stream-to-string (getf params :request-body)))
            (json (json:decode-json-from-string body))
            (name (cdr (assoc :name json)))
            (email (cdr (assoc :email json))))
       (if (and name email)
           (let ((new-user (create-user name email)))
             (list :status 201 :body (json:encode new-user) :headers '((content-type . "application/json"))))
           (list :status 400 :body "Missing name or email" :headers '((content-type . "text/plain"))))))
    (:PUT
     (let* ((id (parse-integer (getf params :path-info) :junk-allowed t)) 
            (body (flex:flex-stream-to-string (getf params :request-body)))
            (json (json:decode-json-from-string body))
            (name (cdr (assoc :name json)))
            (email (cdr (assoc :email json))))
       (if (and id name email)
           (if (get-user id)
               (progn
                 (update-user id name email)
                 (list :status 204 :body "" :headers nil))
               (list :status 404 :body "User not found" :headers '((content-type . "text/plain"))))
           (list :status 400 :body "Missing user ID, name, or email" :headers '((content-type . "text/plain"))))))
    (:DELETE
     (let ((id (parse-integer (getf params :path-info) :junk-allowed t)))
       (if id
           (if (get-user id)
               (progn
                 (delete-user id)
                 (list :status 204 :body "" :headers nil))
               (list :status 404 :body "User not found" :headers '((content-type . "text/plain"))))
           (list :status 400 :body "Invalid user ID" :headers '((content-type . "text/plain"))))))
    (otherwise
     (list :status 405 :body "Method not allowed" :headers '((content-type . "text/plain")))))
  )

(defun dispatch (params)
  (cond
   ((string= (getf params :path-info) "/users")
    (handle-users params))
   ((and (>= (length (getf params :path-info)) 7)
         (string= (subseq (getf params :path-info) 0 6) "/users/"))
    (let ((new-path (subseq (getf params :path-info) 7)))
      (handle-users (append params (list :path-info new-path)))))
   (t
    (list :status 404 :body "Not found" :headers '((content-type . "text/plain"))))))

(defun start-server ()
  (clack:clackup #'dispatch :port 8080))

;;(start-server)
