FROM openjdk:8-jdk-alpine

ENV PGPASSWORD=Saloon5-Moody-Observing

RUN wget -q -O /usr/local/bin/sbt https://raw.githubusercontent.com/sbt/sbt/1.6.2/bin/sbt &&     chmod 0755 /usr/local/bin/sbt

WORKDIR /app

COPY build.sbt .

RUN sbt update

COPY . .

RUN sbt compile

EXPOSE 8080

CMD ["sbt", "run"]
