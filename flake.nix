{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    systems.url = "github:nix-systems/default";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        ./runtimes/rust
        ./runtimes/haskell
        ./snippets/rust
      ];
      # ++ (map (path: ./runtimes/${path}) (with builtins; attrNames (readDir ./runtimes)));

      systems = import inputs.systems;

      perSystem =
        { pkgs, inputs', ... }:

        let
          toolchain =
            with inputs'.fenix.packages;
            combine [
              (fromToolchainFile {
                file = ./rust-toolchain.toml;
                sha256 = "sha256-X/4ZBHO3iW0fOenQ3foEvscgAPJYl2abspaBThDOukI=";
              })
              default.rustfmt # rustfmt nightly
            ];
        in

        {
          devShells.default = pkgs.mkShell {
            packages = [
              toolchain
              pkgs.cargo-nextest
              pkgs.just
              pkgs.nil
              pkgs.nixd
              pkgs.online-judge-tools
            ];
          };
        };

      flake.metadata.neovimFeatures = inputs.nixpkgs.lib.concat [ "rust" ];
    };
}
