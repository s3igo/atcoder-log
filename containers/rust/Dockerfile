FROM nixos/nix:2.21.2

# see https://discourse.nixos.org/t/cross-compilation-failing-with-nix-and-docker-on-macos/22169/4
# when using heredoc, syntax highlighting by treesitter in neovim does not work
RUN echo 'experimental-features = nix-command flakes' >> /etc/nix/nix.conf \
    && echo 'filter-syscalls = false' >> /etc/nix/nix.conf

# setup binary cache
ARG ATTIC_TOKEN
RUN nix run nixpkgs#attic-client -- login fly https://s3igo-nix-cache.fly.dev $ATTIC_TOKEN
RUN nix run nixpkgs#attic-client -- use docker

# setup workspace
WORKDIR /workspace

COPY tasks.nix flake.nix flake.lock rust-toolchain.toml rustfmt.toml Cargo.toml ./
COPY config.toml ./.cargo/

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

# setup fidget.nvim
RUN mkdir -p /root/.cache/nvim

# setup online-judge-tools
ARG ATCODER_USERNAME
ARG ATCODER_PASSWORD
RUN nix develop --command \
    oj login \
    --username $ATCODER_USERNAME \
    --password $ATCODER_PASSWORD \
    -- https://atcoder.jp

# setup environment
ENV RUST_BACKTRACE=1

# pre-build to cache dependencies
RUN mkdir ./src && echo 'fn main() {}' > ./src/main.rs
RUN nix develop --command fish --command 'cargo build && cargo build --release'
RUN rm ./src/main.rs

# start with fish
CMD ["nix", "develop", "--command", "fish"]
