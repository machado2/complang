FROM clfoundation/sbcl:latest

# Install dependencies
RUN apt-get update && apt-get install -y postgresql-client

# Copy source code
COPY . /app

# Install Quicklisp (if not already included in the base image, which it usually is)
RUN apt-get install -y curl
RUN curl -OL https://beta.quicklisp.org/quicklisp.lisp
RUN sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --quit

# Install dependencies using Quicklisp
RUN echo '(ql:quickload :cl-postgres)' | sbcl --load /root/quicklisp/setup.lisp --quit
RUN echo '(ql:quickload :hunchentoot)' | sbcl --load /root/quicklisp/setup.lisp --quit
RUN echo '(ql:quickload :yason)' | sbcl --load /root/quicklisp/setup.lisp --quit
RUN echo '(ql:quickload :ppcre)' | sbcl --load /root/quicklisp/setup.lisp --quit

# Build the application
WORKDIR /app
# Compile, but don't run, during the image build. The server is started in CMD
RUN sbcl --noinform --eval '(ql:quickload :crud-api)' --eval '(asdf:make :crud-api :force t)' --quit

# Expose the port
EXPOSE 8080

# Run the application (start the server)
CMD sbcl --load main.lisp