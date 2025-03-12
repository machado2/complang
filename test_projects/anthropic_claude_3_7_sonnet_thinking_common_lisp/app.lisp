(require :asdf)
(require :uiop)
(require :hunchentoot)
(require :postmodern)
(require :cl-json)

;; Define package for our API
(defpackage :user-api
  (:use :cl :hunchentoot :postmodern :cl-json))

(in-package :user-api)

;; Database connection parameters
(defparameter *db-host* "host.docker.internal")
(defparameter *db-port* 5432)
(defparameter *db-name* "complang")
(defparameter *db-user* "testuser")
(defparameter *db-password* (uiop:getenv "PGPASSWORD"))

;; Connect to database
(defun connect-to-db ()
  (connect-toplevel *db-name* *db-user* *db-password* *db-host* :port *db-port*))

;; Disconnect from database
(defun disconnect-from-db ()
  (disconnect-toplevel))

;; Define the user model
(defclass user ()
  ((id :accessor user-id :initarg :id)
   (name :accessor user-name :initarg :name)
   (email :accessor user-email :initarg :email)))

;; Convert user to JSON
(defun user-to-json (user)
  (with-slots (id name email) user
    (encode-json-to-string 
      (list (cons :id id) 
            (cons :name name) 
            (cons :email email)))))

;; Convert list of users to JSON
(defun users-to-json (users)
  (encode-json-to-string
    (mapcar (lambda (user)
              (with-slots (id name email) user
                (list (cons :id id) 
                      (cons :name name) 
                      (cons :email email))))
            users)))

;; Convert JSON to user parameters
(defun json-to-user-params (json-string)
  (let ((json-object (decode-json-from-string json-string)))
    (values (cdr (assoc :name json-object))
            (cdr (assoc :email json-object)))))

;; Database operations
(defun get-all-users ()
  (query "SELECT id, name, email FROM users"))

(defun get-user-by-id (id)
  (query "SELECT id, name, email FROM users WHERE id = $1" id))

(defun create-user (name email)
  (query "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email" name email))

(defun update-user (id name email)
  (query "UPDATE users SET name = $1, email = $2 WHERE id = $3 RETURNING id" name email id))

(defun delete-user (id)
  (query "DELETE FROM users WHERE id = $1 RETURNING id" id))

;; API handlers
(define-easy-handler (get-users :uri "/users" :method :get) ()
  (setf (content-type*) "application/json")
  (let ((result (get-all-users)))
    (setf (return-code*) 200)
    (users-to-json result)))

(define-easy-handler (get-user :uri "/users/:id" :method :get) (id)
  (setf (content-type*) "application/json")
  (let ((result (get-user-by-id (parse-integer id :junk-allowed t))))
    (if result
        (progn
          (setf (return-code*) 200)
          (users-to-json result))
        (progn
          (setf (return-code*) 404)
          (encode-json-to-string (list (cons :error "User not found")))))))

(define-easy-handler (create-user-handler :uri "/users" :method :post) ()
  (setf (content-type*) "application/json")
  (let ((request-body (raw-post-data :force-text t)))
    (multiple-value-bind (name email) (json-to-user-params request-body)
      (let ((result (create-user name email)))
        (setf (return-code*) 201)
        (users-to-json result)))))

(define-easy-handler (update-user-handler :uri "/users/:id" :method :put) (id)
  (setf (content-type*) "application/json")
  (let ((request-body (raw-post-data :force-text t)))
    (multiple-value-bind (name email) (json-to-user-params request-body)
      (let ((result (update-user (parse-integer id :junk-allowed t) name email)))
        (if result
            (progn
              (setf (return-code*) 200)
              (users-to-json result))
            (progn
              (setf (return-code*) 404)
              (encode-json-to-string (list (cons :error "User not found")))))))))

(define-easy-handler (delete-user-handler :uri "/users/:id" :method :delete) (id)
  (setf (content-type*) "application/json")
  (let ((result (delete-user (parse-integer id :junk-allowed t))))
    (if result
        (progn
          (setf (return-code*) 200)
          (encode-json-to-string (list (cons :success "User deleted"))))
        (progn
          (setf (return-code*) 404)
          (encode-json-to-string (list (cons :error "User not found")))))))

;; Main application entry point
(defun start-server ()
  (connect-to-db)
  (let ((acceptor (make-instance 'easy-acceptor :port 8080)))
    (start acceptor)
    (format t "Server started on port 8080~%")
    acceptor))

;; Start the server when the script is loaded
(defvar *server* (start-server))

;; Keep the script running
(loop
  (sleep 1000))
