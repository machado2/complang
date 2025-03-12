FROM ubuntu:20.04
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y g++ make wget libpqxx-dev libpq-dev
WORKDIR /app
# Download the Crow framework header from an alternative URL
RUN wget -O crow_all.h https://raw.githubusercontent.com/ipkn/crow/master/include/crow_all.h
COPY main.cpp /app/main.cpp
RUN g++ -std=c++17 main.cpp -lpqxx -lpq -o app
EXPOSE 8080
CMD ["./app"]
