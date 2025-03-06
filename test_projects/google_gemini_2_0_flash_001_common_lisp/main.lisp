(in-package :cl-user) ; Explicit package declaration

(defpackage crud-api
  (:use :cl :cl-postgres :hunchentoot :yason :ppcre) ; Explicit and correct package usage
  (:export :main))

(in-package :crud-api) ; Switch to the crud-api package

(defparameter *db-host* "host.docker.internal")
(defparameter *db-port* 5432)
(defparameter *db-name* "complang")
(defparameter *db-user* "testuser")
(defparameter *db-pass* (or (uiop:getenv "PGPASSWORD") "Saloon5-Moody-Observing"))

(defun connect-db ()
  (cl-postgres:connect :host *db-host* :port *db-port* :database *db-name* :user *db-user* :password *db-pass*))

(defun query (sql &optional params)
  (with-connection (db (connect-db))
    (cl-postgres:exec-query db sql params)))

(defun fetch-all (result)
  (loop for row across (cl-postgres:fetch-rows result)
        collect (loop for i from 0 below (length row)
                      collect (elt row i))))

(defun fetch-one (result)
  (let ((row (cl-postgres:fetch-row result)))
    (when row
      (loop for i from 0 below (length row)
            collect (elt row i)))))

(defun get-users ()
  (let ((result (query "SELECT id, name, email FROM users")))
    (mapcar (lambda (row)
              (list :id (parse-integer (car row))
                    :name (cadr row)
                    :email (caddr row)))
            (mapcar (lambda (row) (loop for i from 0 below (length row) collect (elt row i))) (cl-postgres:fetch-rows result)))))

(defun get-user (id)
  (let ((result (query "SELECT id, name, email FROM users WHERE id = $1" (list id))))
    (let ((row (fetch-one result)))
      (when row
        (list :id (parse-integer (car row))
              :name (cadr row)
              :email (caddr row))))))

(defun create-user (name email)
  (let ((result (query "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id" (list name email))))
    (let ((row (fetch-one result)))
      (when row
        (parse-integer (car row))))))

(defun update-user (id name email)
  (query "UPDATE users SET name = $2, email = $3 WHERE id = $1" (list id name email)))

(defun delete-user (id)
  (query "DELETE FROM users WHERE id = $1" (list id)))

(defun json-response (data &optional (status hunchentoot:+http-ok+))
  (hunchentoot:setf-content-type hunchentoot:*reply* "application/json")
  (hunchentoot:set-return-code status)
  (with-output-to-string (s)
    (yason:encode data s)))


(defun handle-users (request)
  (cond
    ((eq (hunchentoot:request-method request) :GET)
     (json-response (get-users)))
    ((eq (hunchentoot:request-method request) :POST)
     (let* ((body (hunchentoot:raw-post-data :request request :external-format hunchentoot:*utf-8-external-format*))
            (json-data (yason:parse body))
            (name (cdr (assoc :name json-data :test 'equal)))
            (email (cdr (assoc :email json-data :test 'equal))))
       (if (and name email)
           (let ((id (create-user name email)))
             (json-response (list :id id :name name :email email) hunchentoot:+http-created+))
           (hunchentoot:set-return-code hunchentoot:+http-bad-request+))))
    (t
     (hunchentoot:set-return-code hunchentoot:+http-method-not-allowed+))))


(defun handle-user (request id)
  (let ((user (get-user id)))
    (cond
      ((null user)
       (hunchentoot:set-return-code hunchentoot:+http-not-found+))
      ((eq (hunchentoot:request-method request) :GET)
       (json-response user))
      ((eq (hunchentoot:request-method request) :PUT)
       (let* ((body (hunchentoot:raw-post-data :request request :external-format hunchentoot:*utf-8-external-format*))
              (json-data (yason:parse body))
              (name (cdr (assoc :name json-data :test 'equal)))
              (email (cdr (assoc :email json-data :test 'equal))))
         (if (and name email)
             (progn
              (update-user id name email)
              (hunchentoot:set-return-code hunchentoot:+http-no-content+))
              (hunchentoot:set-return-code hunchentoot:+http-bad-request+))))
      ((eq (hunchentoot:request-method request) :DELETE)
       (delete-user id)
       (hunchentoot:set-return-code hunchentoot:+http-no-content+))
      (t
       (hunchentoot:set-return-code hunchentoot:+http-method-not-allowed+)))))

(defun dispatch (request)
  (let ((path (hunchentoot:request-uri request)))
    (cond
      ((string= path "/users")
       (handle-users request))
      ((ppcre:scan-to-strings "^/users/(\\d+)$" path :register 0)
       (let ((id (parse-integer (ppcre:scan-to-strings "^/users/(\\d+)$" path :register 1))))
         (handle-user request id)))
      (t
       (hunchentoot:set-return-code hunchentoot:+http-not-found+)))))

(defun main ()
  (format t "Starting server on port 8080~%")
  (let ((acceptor (make-instance 'hunchentoot:easy-acceptor :port 8080)))
    (hunchentoot:define-easy-handler (dispatch :uri-prefix "/") (request)
      (dispatch request))
    (hunchentoot:start acceptor)
    (loop do (sleep 1))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless *load-truename*
       (main)))