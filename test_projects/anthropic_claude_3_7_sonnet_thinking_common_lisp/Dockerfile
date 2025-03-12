FROM debian:bookworm-slim

# Install Steel Bank Common Lisp (SBCL) and other dependencies
RUN apt-get update && apt-get install -y     sbcl     wget     ca-certificates     build-essential     libssl-dev     && rm -rf /var/lib/apt/lists/*

# Set up working directory
WORKDIR /app

# Install Quicklisp (Common Lisp package manager)
RUN wget https://beta.quicklisp.org/quicklisp.lisp     && sbcl --load quicklisp.lisp        --eval '(quicklisp-quickstart:install)'        --eval '(ql:add-to-init-file)'        --eval '(quit)'     && rm quicklisp.lisp

# Copy application code
COPY app.lisp /app/

# Create a Common Lisp script to install dependencies
RUN echo '(load "~/quicklisp/setup.lisp") (ql:quickload :hunchentoot) (ql:quickload :postmodern) (ql:quickload :cl-json) (quit)' > /app/install-deps.lisp

# Install dependencies
RUN sbcl --load /app/install-deps.lisp

# Expose the port the API will run on
EXPOSE 8080

# Set the entrypoint command
CMD ["sbcl", "--load", "/app/app.lisp"]
