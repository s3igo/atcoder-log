{
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
      flake-utils,
      fenix,
      nixvim,
      dotfiles,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        overlays = [
          (final: prev: {
            neovim = nixvim.legacyPackages.${system}.makeNixvimWithModule {
              module = {
                imports = [ dotfiles.nixosModules.base ];

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
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; };
        toolchain =
          with fenix.packages.${system};
          combine [
            (fromToolchainFile {
              file = ./rust-toolchain.toml;
              sha256 = "sha256-gdYqng0y9iHYzYPAdkC/ka3DRny3La/S5G8ASj0Ayyc=";
            })
            default.rustfmt # rustfmt nightly
          ];
        tasks =
          with pkgs;
          let
            test = writeShellApplication {
              name = "t";
              runtimeInputs = [ online-judge-tools ];
              text = ''
                oj test --command 'cargo run --release'
              '';
            };
            submit = writeShellApplication {
              name = "s";
              runtimeInputs = [ online-judge-tools ];
              text = ''
                oj submit ./src/main.rs
              '';
            };
            testAndSubmit = writeShellApplication {
              name = "ts";
              runtimeInputs = [
                test
                submit
              ];
              text = ''
                t && s
              '';
            };
            nvim = writeShellApplication {
              name = "v";
              runtimeInputs = [ neovim ];
              text = ''
                nvim ./src/main.rs
              '';
            };
          in
          [
            test
            submit
            testAndSubmit
            nvim
          ];
      in
      {
        packages = {
          inherit toolchain;
        };

        devShells.default = pkgs.mkShell {
          buildInputs =
            with pkgs;
            [
              online-judge-tools
              fish
              neovim
            ]
            ++ [ toolchain ]
            ++ tasks;
        };
      }
    );
}
