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
        pkgs = import nixpkgs { inherit system; };
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
            build = writeShellScriptBin "task_build" ''
              declare PROJ_ROOT=$(git rev-parse --show-toplevel)
              docker build \
                --build-arg ATTIC_TOKEN=$(sudo cat /run/agenix/attic-token) \
                --build-arg COPILOT_TOKEN=$(cat $XDG_CONFIG_HOME/github-copilot/hosts.json) \
                --tag s3igo/atcoder-rust \
                $PROJ_ROOT/containers/rust
            '';
            run = writeShellScriptBin "task_run" ''
              docker run --rm -it s3igo/atcoder-rust "$@"
            '';
            update = writeShellScriptBin "task_update" ''
              declare PROJ_ROOT=$(git rev-parse --show-toplevel)
              docker run --rm -it \
                --mount type=bind,source=$PROJ_ROOT/containers/rust/flake.lock,target=/workspace/flake.lock \
                s3igo/atcoder-rust \
                nix flake update
            '';
            new = writeShellScriptBin "task_new" ''
              [ -f $1 ] || cat > $1 <<EOF
              use proconio::input;

              fn main() {
                  input!();
              }
              EOF

              declare PROJ_ROOT=$(git rev-parse --show-toplevel)
              docker run --rm -it \
                --mount type=bind,source=$(pwd)/$1,target=/workspace/src/main.rs \
                s3igo/atcoder-rust
            '';
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
