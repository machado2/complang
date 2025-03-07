
FROM haskell:latest

WORKDIR /usr/src/app

COPY app.hs ./

RUN stack setup && stack install

CMD ["stack", "run"]
