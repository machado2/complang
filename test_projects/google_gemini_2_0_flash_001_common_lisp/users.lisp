
(ql:quickload :clack)
(ql:quickload :postmodern)
(ql:quickload :jonathan)

(defpackage users-api
  (:use :cl :clack :postmodern :jonathan))

(in-package users-api)

(defparameter *db-user* "testuser")
(defparameter *db-pass* (uiop:getenv "PGPASSWORD"))
(defparameter *db-name* "complang")
(defparameter *db-host* "host.docker.internal")
(defparameter *db-port* 5432)

(defun db-connect ()
  (connect *db-name* *db-user* *db-pass* *db-host* :port *db-port*))

(defun get-users ()
  (db-connect)
  (let ((users (query "SELECT id, name, email FROM users")))
    (disconnect)
    (mapcar (lambda (user)
              (jonathan:to-json
               (list :id (getf user :id) :name (getf user :name) :email (getf user :email))))
            users)))

(defun get-user (id)
  (db-connect)
  (let ((user (query "SELECT id, name, email FROM users WHERE id = $1" id)))
    (disconnect)
    (if user
        (jonathan:to-json
         (list :id (getf (first user) :id) :name (getf (first user) :name) :email (getf (first user) :email)))
        nil)))

(defun create-user (name email)
  (db-connect)
  (let ((id (execute "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id" name email)))
    (disconnect)
    (getf (first id) :id)))

(defun update-user (id name email)
  (db-connect)
  (execute "UPDATE users SET name = $2, email = $3 WHERE id = $1" id name email)
  (disconnect))

(defun delete-user (id)
  (db-connect)
  (execute "DELETE FROM users WHERE id = $1" id)
  (disconnect))

(defun handle-users-route (request)
  (case (request-method request)
    (:GET
     (format nil "~A" (jonathan:to-json (mapcar #'jonathan:parse-json (get-users)))))
    (:POST
     (let* ((body (slurp-input-stream (request-body request)))
            (json-body (jonathan:parse-json body))
            (name (cdr (assoc :name json-body)))
            (email (cdr (assoc :email json-body))))
       (if (and name email)
           (let ((new-user-id (create-user name email)))
             (setf (response-status request) 201)
             (format nil "~A" (get-user new-user-id)))
           (progn
             (setf (response-status request) 400)
             "Invalid input"))))
    (otherwise
     (setf (response-status request) 405)
     "Method Not Allowed")))

(defun handle-user-route (request id)
  (if (not (stringp id))
      (setf id (parse-integer id)))
  (case (request-method request)
    (:GET
     (let ((user (get-user id)))
       (if user
           (format nil "~A" user)
           (progn
             (setf (response-status request) 404)
             "User not found"))))
    (:PUT
     (let* ((body (slurp-input-stream (request-body request)))
            (json-body (jonathan:parse-json body))
            (name (cdr (assoc :name json-body)))
            (email (cdr (assoc :email json-body))))
       (if (and name email)
           (progn
             (update-user id name email)
             (setf (response-status request) 204)
             "")
           (progn
             (setf (response-status request) 400)
             "Invalid input"))))
    (:DELETE
     (if (get-user id)
         (progn
           (delete-user id)
           (setf (response-status request) 204)
           "")
         (progn
           (setf (response-status request) 404)
           "User not found")))
    (otherwise
     (setf (response-status request) 405)
     "Method Not Allowed")))

(defroute *routes*
  ("/users" #'handle-users-route)
  ("/users/:id" #'handle-user-route))

(defun main (&rest args)
  (declare (ignore args))
  (db-connect) ; Test database connection
  (disconnect)
  (clack:clackup
   (lambda (request)
     (destructuring-bind (status headers body)
         (if (route *routes* request)
             (multiple-value-list (route *routes* request))
             (list 404 '(:content-type "text/plain") '("Not Found")) )
       (list status headers body))
     )
   :port 8080))

(uiop:register-image-dump-hook
 #'(lambda ()
     (format t "Shutting down Clack...~%")
     (clack:stop *server*))
 :quit-hook t)

(eval (main))
