FROM openjdk:11-jdk-slim

WORKDIR /app

COPY build.sbt .
COPY project ./project

RUN apt-get update && apt-get install -y --no-install-recommends apt-transport-https
RUN apt-get update && apt-get install -y gnupg
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | tee /etc/apt/sources.list.d/sbt_new.list
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823
RUN apt-get update
RUN apt-get install -y sbt

COPY src ./src

RUN sbt assembly

EXPOSE 8080

CMD ["java", "-jar", "target/scala-2.13/scala-crud-assembly-1.0.jar"]