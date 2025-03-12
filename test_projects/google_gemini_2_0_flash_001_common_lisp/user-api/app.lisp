
(ql:quickload :clack)
(ql:quickload :cl-json)
(ql:quickload :user-api.db)
(ql:quickload :alexandria)

(defpackage :user-api.app
  (:use :cl :clack :cl-json :user-api.db :alexandria)
  (:export :start-app))

(in-package :user-api.app)

(defun handle-users-route (request)
  (case (request-method request)
    (:GET
     (let ((users (get-all-users)))
       (with-output-to-string (s)
         (write-json users :stream s)
         (list 200 '(:content-type "application/json") (list s)))))
    (:POST
     (let* ((body (clack.request:request-body request))
            (body-string (babel:octets-to-string body :encoding :utf-8))
            (json-body (json:decode-json-from-string body-string))
            (name (json:getf json-body :name))
            (email (json:getf json-body :email)))
        (if (and name email)
            (let ((new-user-id (create-user name email)))
              (if new-user-id
                  (let ((new-user (get-user-by-id new-user-id)))
                    (with-output-to-string (s)
                      (write-json new-user :stream s)
                      (list 201 '(:content-type "application/json") (list s))))
                  (list 500 '(:content-type "text/plain") (list "Failed to create user"))))
            (list 400 '(:content-type "text/plain") (list "Missing name or email")))))
    (otherwise (list 405 '(:content-type "text/plain") (list "Method Not Allowed")))))


(defun handle-user-id-route (id request)
  (let ((user (get-user-by-id id)))
    (if user
        (case (request-method request)
          (:GET
           (with-output-to-string (s)
             (write-json user :stream s)
             (list 200 '(:content-type "application/json") (list s))))
          (:PUT
           (let* ((body (clack.request:request-body request))
                  (body-string (babel:octets-to-string body :encoding :utf-8))
                  (json-body (json:decode-json-from-string body-string))
                  (name (json:getf json-body :name))
                  (email (json:getf json-body :email)))
             (if (and name email)
                 (let ((rows-updated (update-user id name email)))
                   (if (> rows-updated 0)
                       (list 204 '(:content-type "text/plain") (list ""))
                       (list 500 '(:content-type "text/plain") (list "Failed to update user"))))
                 (list 400 '(:content-type "text/plain") (list "Missing name or email")))))
          (:DELETE
           (let ((rows-deleted (delete-user id)))
             (if (> rows-deleted 0)
                 (list 204 '(:content-type "text/plain") (list ""))
                 (list 500 '(:content-type "text/plain") (list "Failed to delete user")))))
          (otherwise (list 405 '(:content-type "text/plain") (list "Method Not Allowed"))))
        (list 404 '(:content-type "text/plain") (list "User not found")))))

(defun app (env)
  (let ((request (clack.request:make-request env))
        (path (clack.request:request-path request)))
    (cond
      ((string= path "/users") (handle-users-route request))
      ((starts-with-subseq "/users/" path)
       (let ((id (parse-integer (subseq path 7) :junk-allowed t)))
         (if id
             (handle-user-id-route id request)
             (list 400 '(:content-type "text/plain") (list "Invalid user ID")))))
      (t (list 404 '(:content-type "text/plain") (list "Not Found"))))))

(defun start-app ()
  (clack:clackup #'app :port 8080))
