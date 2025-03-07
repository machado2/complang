
FROM dlang/dlang

WORKDIR /app

COPY . .

RUN dmd main.d -of=api -L-lvibe -L-lvibe_db_postgres

CMD ["/app/api"]
EXPOSE 8080
