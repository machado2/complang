
FROM clfoundation/sbcl

RUN apt-get update && apt-get install -y curl && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY app.lisp .
ENV PPORT 8080

RUN apt-get update && apt-get install -y curl
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp
RUN sbcl --noinform --load quicklisp.lisp --eval '(quicklisp-quickstart:install)'
RUN sbcl --noinform --eval '(ql:quickload :clack)'

CMD sbcl --eval '(load "app.lisp")'
