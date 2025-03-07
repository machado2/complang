
FROM drakma/sbcl

RUN apt-get update -y
RUN apt-get install -y postgresql-client curl

# Install quicklisp
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp
RUN sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --quit
RUN ln -s /root/.quicklisp/bin/ql /usr/local/bin/ql

WORKDIR /app

COPY users-api.asd .
COPY users.lisp .

# Install dependencies
RUN ql install users-api

# Load and run the application
CMD ql eval '(users-api:main)'
EXPOSE 8080
