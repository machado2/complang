
FROM openjdk:11-jre-slim
RUN apt-get update && apt-get install -y apt-transport-https curl gnupg wget ca-certificates jq
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | tee /etc/apt/sources.list.d/sbt_old.list
RUN curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | gpg --no-default-keyring --keyring gnupg-ring:/etc/apt/trusted.gpg.d/scalasbt-release.gpg --import
RUN chmod 644 /etc/apt/trusted.gpg.d/scalasbt-release.gpg
RUN apt-get update && apt-get install -y sbt
WORKDIR /app
COPY . .
RUN sbt assembly
RUN mkdir -p target/scala-2.13/
RUN cp src/main/resources/application.conf target/scala-2.13/application.conf
EXPOSE 8080
ENTRYPOINT ["java", "-Dconfig.file=target/scala-2.13/application.conf", "-jar", "target/scala-2.13/app.jar"]
