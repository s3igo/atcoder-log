FROM atcoder/base

# setup workspace
WORKDIR /workspace
COPY tasks.nix flake.nix flake.lock rust-toolchain.toml rustfmt.toml Cargo.toml ./
COPY config.toml ./.cargo/
COPY rust.code-snippets ./.vscode/

# download `Cargo.lock`
RUN curl https://raw.githubusercontent.com/rust-lang-ja/atcoder-proposal/fe6aa6179d074d3a565d3c3db256db54071a38f9/Cargo.lock -fO

# build `devShells.default`
RUN nix develop --profile /tmp/dev-shell --command true
RUN nix run nixpkgs#attic-client -- push docker /tmp/dev-shell
RUN rm -rf /tmp/dev-shell

# setup copilot.lua
ARG COPILOT_TOKEN
RUN mkdir -p /root/.config/github-copilot \
    && echo $COPILOT_TOKEN > /root/.config/github-copilot/hosts.json

# setup environment
ENV RUST_BACKTRACE=1

# pre-build to cache dependencies
RUN mkdir ./src && echo 'fn main() {}' > ./src/main.rs
RUN nix develop --command fish --command 'cargo build && cargo build --release'
RUN rm ./src/main.rs
