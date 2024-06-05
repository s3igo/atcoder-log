FROM atcoder/base

# setup workspace
WORKDIR /workspace
COPY tasks.nix flake.nix flake.lock .merlin .ocamlformat typos.toml ./
COPY ocaml.code-snippets ./.vscode/

# build `devShells.default`
RUN nix develop --profile /tmp/dev-shell --command true
RUN nix run nixpkgs#attic-client -- push docker /tmp/dev-shell
RUN rm -rf /tmp/dev-shell
