
# Dockerfile

FROM ubuntu:20.04

# Install dependencies
RUN apt-get update && apt-get install -y sbcl curl git

# Install Quicklisp
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --load quicklisp.lisp --non-interactive --eval "(progn (load \"quicklisp.lisp\") (quicklisp-quickstart:install) (quit))" && \
    rm quicklisp.lisp

# Set work directory
WORKDIR /app

# Copy the server code into the container
COPY server.lisp /app/server.lisp

# Expose port 8080
EXPOSE 8080

# Start the server using SBCL in script mode
CMD ["sbcl", "--script", "server.lisp"]
