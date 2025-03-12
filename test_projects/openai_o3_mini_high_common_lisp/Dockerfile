FROM ubuntu:20.04
ENV DEBIAN_FRONTEND=noninteractive

# Install dependencies
RUN apt-get update && apt-get install -y sbcl curl git build-essential libpq-dev

# Install Quicklisp
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --non-interactive --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --eval "(quit)"

# Copy the application and build scripts
COPY app.lisp /app/app.lisp
COPY build.lisp /app/build.lisp
WORKDIR /app

# Build a precompiled Lisp executable for fast startup.
RUN sbcl --non-interactive --load build.lisp

# Expose port 8080
EXPOSE 8080

# Run the precompiled executable
CMD ["./app.core"]
