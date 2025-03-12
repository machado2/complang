FROM clojure:openjdk-17-tools-deps-slim-bullseye
WORKDIR /app
COPY deps.edn .
COPY src/ src/
EXPOSE 8080
CMD ["clojure", "-M", "-m", "complang.core"]