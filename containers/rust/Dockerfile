# common config ----------------------------------------------------------------
FROM nixos/nix:2.20.4 as base

# see https://discourse.nixos.org/t/cross-compilation-failing-with-nix-and-docker-on-macos/22169/4
# when using heredoc, syntax highlighting by treesitter in neovim does not work
RUN echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf \
    && echo "filter-syscalls = false" >> /etc/nix/nix.conf

# attic builder ----------------------------------------------------------------
FROM base as attic

# build attic
WORKDIR /tmp/attic
RUN nix build github:zhaofengli/attic#attic-client

# copy store
RUN mkdir /tmp/store
RUN cp -r $(nix-store -qR result/) /tmp/store

# final image ------------------------------------------------------------------
FROM base

# copy result from attic builder
COPY --from=attic /tmp/store /nix/store
COPY --from=attic /tmp/attic/result /attic
ENV PATH=/attic/bin:$PATH

# setup binary cache
ARG ATTIC_TOKEN
RUN attic login fly https://s3igo-nix-cache.fly.dev $ATTIC_TOKEN
RUN attic use docker

# setup workspace
WORKDIR /workspace

COPY flake.nix rust-toolchain.toml Cargo.toml ./
COPY config.toml ./.cargo/

# download `Cargo.lock`
RUN curl https://raw.githubusercontent.com/rust-lang-ja/atcoder-proposal/fe6aa6179d074d3a565d3c3db256db54071a38f9/Cargo.lock -fO

# build devShell
RUN nix develop --profile /tmp/devshell -c true
RUN attic push docker /tmp/devshell

# pre-build
RUN mkdir ./src && echo "fn main() {}" > ./src/main.rs
RUN nix develop --command fish -c "cargo build --release"
RUN rm ./src/main.rs

# start with fish
CMD ["nix", "develop", "--command", "fish"]
