(load "/root/quicklisp/setup.lisp")
(ql:quickload '("hunchentoot" "cl-json" "postmodern" "uiop"))

;; Connect to PostgreSQL using the PGPASSWORD environment variable.
(let ((pgpass (uiop:getenv "PGPASSWORD")))
  (unless pgpass (error "PGPASSWORD env variable not set"))
  (defparameter *db-connection*
    (postmodern:connect :host "host.docker.internal"
                        :port 5432
                        :database "complang"
                        :user "testuser"
                        :password pgpass)))

;; CRUD helper functions
(defun create-user-db (name email)
  (let ((result (postmodern:query "INSERT INTO users (name, email) VALUES (?, ?) RETURNING id" name email)))
    (list :id (first (first result)) :name name :email email)))

(defun get-all-users-db ()
  (let ((results (postmodern:query "SELECT id, name, email FROM users")))
    (mapcar (lambda (row)
              (list :id (elt row 0) :name (elt row 1) :email (elt row 2)))
            results)))

(defun get-user-by-id-db (id)
  (let ((results (postmodern:query "SELECT id, name, email FROM users WHERE id = ?" id)))
    (when results
      (let ((row (first results)))
        (list :id (elt row 0) :name (elt row 1) :email (elt row 2))))))

(defun update-user-db (id name email)
  (let ((result (postmodern:query "UPDATE users SET name = ?, email = ? WHERE id = ? RETURNING id" name email id)))
    (if result t nil)))

(defun delete-user-db (id)
  (let ((result (postmodern:query "DELETE FROM users WHERE id = ? RETURNING id" id)))
    (if result t nil)))

;; HTTP endpoint definitions using Hunchentoot
(define-easy-handler (post-users :method :post :uri "/users") ()
  (let* ((body (hunchentoot:raw-post-data*))
         (data (cl-json:decode-json-from-string body))
         (name (cdr (assoc "name" data :test #'string=)))
         (email (cdr (assoc "email" data :test #'string=))))
    (if (and name email)
        (let ((user (create-user-db name email)))
          (setf (hunchentoot:content-type*) "application/json")
          (hunchentoot:set-response-status 201)
          (cl-json:encode-json-to-string user))
        (progn
          (hunchentoot:set-response-status 400)
          "Invalid user data."))))

(define-easy-handler (get-users :method :get :uri "/users") ()
  (setf (hunchentoot:content-type*) "application/json")
  (cl-json:encode-json-to-string (get-all-users-db)))

(define-easy-handler (get-user :method :get :uri-regexp "/users/\([0-9]+\)$") ()
  (let* ((id-str (first (hunchentoot:dispatch-arguments*)))
         (id (parse-integer id-str))
         (user (get-user-by-id-db id)))
    (if user
        (progn
          (setf (hunchentoot:content-type*) "application/json")
          (cl-json:encode-json-to-string user))
        (progn
          (hunchentoot:set-response-status 404)
          "User not found."))))

(define-easy-handler (put-user :method :put :uri-regexp "/users/\([0-9]+\)$") ()
  (let* ((id-str (first (hunchentoot:dispatch-arguments*)))
         (id (parse-integer id-str))
         (body (hunchentoot:raw-post-data*))
         (data (cl-json:decode-json-from-string body))
         (name (cdr (assoc "name" data :test #'string=)))
         (email (cdr (assoc "email" data :test #'string=))))
    (if (and name email)
        (if (update-user-db id name email)
            (progn
              (hunchentoot:set-response-status 200)
              "User updated.")
            (progn
              (hunchentoot:set-response-status 404)
              "User not found."))
        (progn
          (hunchentoot:set-response-status 400)
          "Invalid user data."))))

(define-easy-handler (delete-user :method :delete :uri-regexp "/users/\([0-9]+\)$") ()
  (let* ((id-str (first (hunchentoot:dispatch-arguments*)))
         (id (parse-integer id-str)))
    (if (delete-user-db id)
        (progn
          (hunchentoot:set-response-status 200)
          "User deleted.")
        (progn
          (hunchentoot:set-response-status 404)
          "User not found."))))

;; The main function that starts the Hunchentoot server on port 8080.
(defun main ()
  (setf hunchentoot:*default-port* 8080)
  (hunchentoot:start (make-instance 'hunchentoot:acceptor))
  (format t "Server started on port 8080~%")
  (force-output))
