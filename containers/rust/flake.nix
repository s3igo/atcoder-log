{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    dotfiles.url = "github:s3igo/dotfiles";
  };

  outputs =
    {
      nixpkgs,
      flake-utils,
      fenix,
      dotfiles,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        overlays = [
          (final: prev: {
            neovim = dotfiles.neovim.${system} {
              inherit pkgs;
              modules = with dotfiles.nixosModules; [ rust ];
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
        rustfmt-config = pkgs.stdenv.mkDerivation {
          name = "rustfmt-config";
          src = ./rustfmt.toml;
          phases = [ "installPhase" ];
          installPhase = ''
            mkdir -p $out
            cp $src $out/rustfmt.toml
          '';
        };
        tasks = import ./tasks.nix { inherit pkgs; };
      in
      {
        packages = {
          inherit toolchain rustfmt-config;
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
