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
        pkgs = import nixpkgs { inherit system; };
        deps =
          let
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
            toolchain =
              with fenix.packages.${system};
              combine [
                (fromToolchainFile {
                  file = ./rust-toolchain.toml;
                  sha256 = "sha256-gdYqng0y9iHYzYPAdkC/ka3DRny3La/S5G8ASj0Ayyc=";
                })
                default.rustfmt # rustfmt nightly
              ];
          in
          [
            neovim
            toolchain
          ];
        tasks =
          let
            submit = pkgs.writeShellApplication {
              name = "s";
              runtimeInputs = [ pkgs.online-judge-tools ];
              text = ''
                oj test && oj submit
              '';
            };
          in
          [ submit ];
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs =
            with pkgs;
            [
              online-judge-tools
              fish
            ]
            ++ deps
            ++ tasks;
        };
      }
    );
}
