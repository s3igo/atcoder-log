FROM rust:slim-bullseye

RUN rustup toolchain install 1.42

RUN apt update && apt install -y \
  git \
  libssl-dev \
  pkg-config \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /workspace
RUN cargo install cargo-compete
