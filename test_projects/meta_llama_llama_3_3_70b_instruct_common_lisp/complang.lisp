
(ql:quickload :cl-postgres)
(ql:quickload :hunchentoot)

(defpackage :complang
  (:use :cl :cl-postgres :hunchentoot))

(in-package :complang)

(defvar *db-host* "host.docker.internal")
(defvar *db-port* 5432)
(defvar *db-name* "complang")
(defvar *db-user* "testuser")
(defvar *db-password* (uiop:getenv "PGPASSWORD"))

(defun connect-to-db ()
  (cl-postgres:connect *db-host* *db-port* *db-name* *db-user* *db-password*))

(defun create-user (name email)
  (let ((conn (connect-to-db)))
    (cl-postgres:execute conn "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING *" (list name email))
    (cl-postgres:close conn)))

(defun get-users ()
  (let ((conn (connect-to-db)))
    (cl-postgres:execute conn "SELECT * FROM users")
    (cl-postgres:close conn)))

(defun get-user (id)
  (let ((conn (connect-to-db)))
    (cl-postgres:execute conn "SELECT * FROM users WHERE id = $1" (list id))
    (cl-postgres:close conn)))

(defun update-user (id name email)
  (let ((conn (connect-to-db)))
    (cl-postgres:execute conn "UPDATE users SET name = $1, email = $2 WHERE id = $3" (list name email id))
    (cl-postgres:close conn)))

(defun delete-user (id)
  (let ((conn (connect-to-db)))
    (cl-postgres:execute conn "DELETE FROM users WHERE id = $1" (list id))
    (cl-postgres:close conn)))

(define-easy-handler (users :uri "/users") ()
  (let ((method (request-method *request*)))
    (cond
      ((equal method :post)
       (let ((name (parameter "name"))
             (email (parameter "email")))
         (create-user name email)
         (return-json "{\"id\": 1, \"name\": \"\"}" :status 201)))
      ((equal method :get)
       (let ((users (get-users)))
         (return-json (format nil "[~{\{\"id\": ~a, \"name\": \"~a\", \"email\": \"~a\"\}~^, ~}" (loop for row in users collect (list (first row) (second row) (third row)))) :status 200)))
      (t (return-json "{\"error\": \"Method not allowed\"}" :status 405)))))

(define-easy-handler (user :uri "/users/:id") (id)
  (let ((method (request-method *request*)))
    (cond
      ((equal method :get)
       (let ((user (get-user id)))
         (if user
             (return-json (format nil "{\"id\": ~a, \"name\": \"~a\", \"email\": \"~a\"}" (first user) (second user) (third user)) :status 200)
             (return-json "{\"error\": \"User not found\"}" :status 404))))
      ((equal method :put)
       (let ((name (parameter "name"))
             (email (parameter "email")))
         (update-user id name email)
         (return-json "{\"message\": \"User updated\"}" :status 200)))
      ((equal method :delete)
       (delete-user id)
       (return-json "{\"message\": \"User deleted\"}" :status 200))
      (t (return-json "{\"error\": \"Method not allowed\"}" :status 405)))))

(start-server :port 8080)
