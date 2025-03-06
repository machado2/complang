FROM ubuntu:latest

RUN apt-get update && apt-get install -y --no-install-recommends curl postgresql-client jq xz-utils

RUN apt-get update && apt-get install -y wget
RUN wget https://ziglang.org/builds/zig-linux-x86_64-0.14.0.tar.xz
RUN tar -xf zig-linux-x86_64-0.14.0.tar.xz && \
    mv zig-linux-x86_64-0.14.0 /opt/zig && \
    ln -s /opt/zig/zig /usr/local/bin/zig

WORKDIR /app
COPY . .

EXPOSE 8080
CMD [ "zig", "run", "src/main.zig" ]