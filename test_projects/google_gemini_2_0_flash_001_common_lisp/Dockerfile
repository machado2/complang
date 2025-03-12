
FROM clojure:latest

# Install Quicklisp
RUN apt-get update && apt-get install -y curl
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp
RUN clojure -M -e "(load "quicklisp.lisp")"
RUN clojure -M -e "(quicklisp-quickstart:install)"

# Copy the Lisp code
WORKDIR /app
COPY complang.lisp .

# Install dependencies
RUN clojure -Sdeps '{:deps {org.clojure/clojure {:mvn/version "1.11.1"} postgresql/postgresql {:mvn/version "9.4-1206-jdbc42"}  }' -e nil

# Set the entrypoint
CMD ["clojure", "complang.lisp"]
