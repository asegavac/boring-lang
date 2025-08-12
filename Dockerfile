FROM rust:1.89

# RUN apt update && apt-get install -y llvm clang
RUN rustup component add rustfmt
WORKDIR /code
