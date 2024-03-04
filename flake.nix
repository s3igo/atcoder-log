{
  description = "AtCoder development environment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixvim = {
      url = "github:nix-community/nixvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    dotfiles.url = "github:s3igo/dotfiles";
  };

  outputs =
    {
      self,
      nixpkgs,
      fenix,
      nixvim,
      dotfiles,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };
        neovim = nixvim.legacyPackages.${system}.makeNixvimWithModule {
          module = {
            imports = [ dotfiles.nixosModules.neovim ];

            plugins.lsp.servers.rust-analyzer = {
              enable = true;
              installCargo = false;
              installRustc = false;
              settings = {
                check.command = "clippy";
                files.excludeDirs = [ ".direnv" ];
              };
            };
          };
        };
        deps =
          let
            toolchain =
              with fenix.packages.${system};
              combine [
                (fromToolchainFile {
                  file = ./contests/1.70.0/rust-toolchain.toml;
                  sha256 = "sha256-gdYqng0y9iHYzYPAdkC/ka3DRny3La/S5G8ASj0Ayyc=";
                })
                default.rustfmt # rustfmt nightly
              ];
            cargo-compete = import ./cargo-compete.nix { inherit pkgs; };
            cargo-snippet = import ./cargo-snippet.nix { inherit pkgs; };
          in
          [
            toolchain
            cargo-compete
            cargo-snippet
          ];
        tasks =
          with pkgs;
          let
            build = writeShellApplication {
              name = "task_build";
              runtimeInputs = [ _1password ];
              text = ''
                PROJ_ROOT=$(git rev-parse --show-toplevel)

                docker build \
                  --build-arg ATTIC_TOKEN="$(sudo cat /run/agenix/attic-token)" \
                  --build-arg COPILOT_TOKEN="$(cat "$XDG_CONFIG_HOME/github-copilot/hosts.json")" \
                  --build-arg ATCODER_USERNAME="$(op read op://Personal/AtCoder/username)" \
                  --build-arg ATCODER_PASSWORD="$(op read op://Personal/AtCoder/password)" \
                  --tag s3igo/atcoder-rust \
                  "$PROJ_ROOT/containers/rust"
              '';
            };
            run = writeShellApplication {
              name = "task_run";
              text = ''
                docker run --rm -it s3igo/atcoder-rust "$@"
              '';
            };
            update = writeShellApplication {
              name = "task_update";
              text = ''
                PROJ_ROOT=$(git rev-parse --show-toplevel)
                docker run --rm -it \
                  --mount type=bind,source="$PROJ_ROOT/containers/rust/flake.lock",target=/workspace/flake.lock \
                  s3igo/atcoder-rust \
                  nix flake update
              '';
            };
            new = writeShellApplication {
              name = "task_new";
              text = ''
                # $1: task url (optional)
                # $2: filename (optional)
                # at least one of them is required
                [[ $1 == https://atcoder.jp/* ]] \
                  && FILENAME=''${2:-$(basename "$1").rs} \
                  || FILENAME="$1"

                [ -f "$FILENAME" ] || cat > "$FILENAME" <<EOF
                use proconio::input;

                fn main() {
                    input!();
                }
                EOF

                # redundant command due to the complexity of [COMMAND] and [ARG...]
                # received by `docker run`
                if [[ $1 == https://atcoder.jp/* ]]; then
                  docker run --rm -it \
                    --mount type=bind,source="$(pwd)/$FILENAME",target=/workspace/src/main.rs \
                    s3igo/atcoder-rust \
                    nix develop --command fish --init-command "oj download $1 && nvim src/main.rs"
                else
                  docker run --rm -it \
                    --mount type=bind,source="$(pwd)/$FILENAME",target=/workspace/src/main.rs \
                    s3igo/atcoder-rust \
                    nix develop --command fish --init-command 'nvim src/main.rs'
                fi
              '';
            };
          in
          [
            build
            run
            update
            new
          ];
      in
      {
        packages = {
          inherit neovim;
          default = neovim;
        };

        devShells.default = pkgs.mkShell { buildInputs = deps ++ tasks; };
      }
    );
}
