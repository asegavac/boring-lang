FROM rust:1.39

RUN apt update && apt-get install -y llvm clang
WORKDIR /code
