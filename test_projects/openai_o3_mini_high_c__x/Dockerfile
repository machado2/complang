FROM gcc:10

RUN apt-get update && apt-get install -y libpq-dev libpqxx-dev git

WORKDIR /app

COPY main.cpp .

# Clone the ipkn/crow repository and copy the crow_all.h header
RUN git clone --depth 1 https://github.com/ipkn/crow.git && cp crow/include/crow_all.h .

# Build the application
RUN g++ -O2 -std=c++17 main.cpp -o app -lpqxx -lpq -pthread

EXPOSE 8080

CMD ["./app"]