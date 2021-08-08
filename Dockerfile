FROM rust:1.54

RUN apt update && apt-get install -y llvm clang
WORKDIR /code
