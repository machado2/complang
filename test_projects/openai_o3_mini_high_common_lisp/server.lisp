
;;;; server.lisp
;;;; A Common Lisp CRUD API using Hunchentoot, CL-JSON, and CL-Postgres

;; Load Quicklisp
(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(ql:quickload '(:hunchentoot :cl-json :cl-postgres :uiop))

(defpackage :complang-api
  (:use :cl :hunchentoot :cl-json :cl-postgres :uiop))
(in-package :complang-api)

;; Global variable for the database connection
(defvar *db-connection* nil)

(defun init-db-connection ()
  "Initialize the PostgreSQL database connection using the PGPASSWORD env var."
  (let* ((pgpassword (or (uiop:getenv "PGPASSWORD") ""))
         (conn-str (format nil "host=host.docker.internal port=5432 dbname=complang user=testuser password=~a" pgpassword)))
    (setf *db-connection* (cl-postgres:connect conn-str))
    (format t "Connected to database complang~%")))

(defun create-user (name email)
  "Insert a new user into the database and return the created row."
  (car (cl-postgres:query *db-connection*
         "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email"
         (list name email))))

(defun list-users ()
  "Retrieve all users from the database."
  (cl-postgres:query *db-connection* "SELECT id, name, email FROM users"))

(defun get-user (id)
  "Retrieve a single user by id."
  (first (cl-postgres:query *db-connection*
           "SELECT id, name, email FROM users WHERE id=$1"
           (list id))))

(defun update-user (id name email)
  "Update an existing user; returns the number of rows affected."
  (cl-postgres:execute *db-connection*
    "UPDATE users SET name=$1, email=$2 WHERE id=$3"
    (list name email id)))

(defun delete-user (id)
  "Delete a user by id; returns the number of rows affected."
  (cl-postgres:execute *db-connection*
    "DELETE FROM users WHERE id=$1"
    (list id)))

;;;; HTTP Handlers

(define-easy-handler (create-user-handler :uri "/users" :method :post) ()
  (setf (hunchentoot:content-type*) "application/json")
  (let* ((body (hunchentoot:raw-post-data*))
         (data (cl-json:decode-json-from-string body))
         (name (gethash "name" data))
         (email (gethash "email" data))
         (user (create-user name email)))
    (setf (hunchentoot:reply-status*) 201)
    (cl-json:encode-json-to-string user)))

(define-easy-handler (list-users-handler :uri "/users" :method :get) ()
  (setf (hunchentoot:content-type*) "application/json")
  (cl-json:encode-json-to-string (list-users)))

(define-easy-handler (get-user-handler :uri-regex "^/users/([0-9]+)$" :method :get) (user-id)
  (let* ((id (parse-integer user-id))
         (user (get-user id)))
    (if user
        (progn
          (setf (hunchentoot:content-type*) "application/json")
          (cl-json:encode-json-to-string user))
        (progn
          (setf (hunchentoot:reply-status*) 404)
          "User not found"))))

(define-easy-handler (update-user-handler :uri-regex "^/users/([0-9]+)$" :method :put) (user-id)
  (let ((id (parse-integer user-id)))
    (let* ((body (hunchentoot:raw-post-data*))
           (data (cl-json:decode-json-from-string body))
           (name (gethash "name" data))
           (email (gethash "email" data))
           (updated (update-user id name email)))
      (if (zerop updated)
          (progn
            (setf (hunchentoot:reply-status*) 404)
            "User not found")
          (progn
            (setf (hunchentoot:reply-status*) 200)
            "User updated")))))

(define-easy-handler (delete-user-handler :uri-regex "^/users/([0-9]+)$" :method :delete) (user-id)
  (let ((id (parse-integer user-id)))
    (let ((deleted (delete-user id)))
      (if (zerop deleted)
          (progn
            (setf (hunchentoot:reply-status*) 404)
            "User not found")
          (progn
            (setf (hunchentoot:reply-status*) 200)
            "User deleted")))))

(defun main ()
  (init-db-connection)
  (let ((server (make-instance 'hunchentoot:easy-acceptor :port 8080)))
    ;; Bind to all interfaces so the API is reachable from outside the container
    (setf (hunchentoot:acceptor-host server) "0.0.0.0")
    (format t "Starting server on port 8080, listening on 0.0.0.0~%")
    (hunchentoot:start server)
    (loop while t do (sleep 1))))
    
;; Start the server when the script is loaded
(main)
