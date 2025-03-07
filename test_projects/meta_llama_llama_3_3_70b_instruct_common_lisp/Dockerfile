
FROM sbcl:latest

WORKDIR /app

COPY complang.lisp /app/complang.lisp

RUN sbcl --load /app/complang.lisp --eval '(ql:quickload :cl-postgres)' --eval '(ql:quickload :hunchentoot)'

EXPOSE 8080

CMD ["sbcl", "--load", "/app/complang.lisp", "--eval", "(start-server :port 8080)"]
