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
        tasks = import ./tasks.nix { inherit nixpkgs system; };
      in
      {
        packages = {
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
        };

        devShells.default = pkgs.mkShell { buildInputs = deps ++ tasks; };
      }
    );
}
