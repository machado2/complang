FROM ubuntu:latest

RUN apt-get update && apt-get install -y \
    curl \
    unzip \
    wget \
    sudo \
    build-essential software-properties-common gnupg

# Add the D language repository using key and source directly.
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys F3F896F3274BBD9BBBA59058710592E7FB7AF6CA && echo "deb http://ppa.launchpad.net/d-community/ppa/ubuntu noble main" > /etc/apt/sources.list.d/d-lang.list && apt-get update && apt-get install -y --no-install-recommends dmd dub

ENV PATH="/usr/local/bin:${PATH}"
WORKDIR /app

COPY dub.json .
COPY source ./source

RUN dub build

EXPOSE 8080

CMD ["./crud_api"]