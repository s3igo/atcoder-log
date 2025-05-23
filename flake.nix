{
  inputs = {
    nixpkgs.url = "https://channels.nixos.org/nixos-unstable/nixexprs.tar.xz";
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
        ./snippets/rust
      ];

      systems = import inputs.systems;

      perSystem =
        { pkgs, inputs', ... }:

        let
          toolchain =
            with inputs'.fenix.packages;
            combine [
              (fromToolchainFile {
                file = ./rust-toolchain.toml;
                sha256 = "sha256-KUm16pHj+cRedf8vxs/Hd2YWxpOrWZ7UOrwhILdSJBU=";
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
              pkgs.zig
              pkgs.zls
              pkgs.online-judge-tools
              (pkgs.callPackage ./runtimes/zig/zlint { })
            ];
          };
        };

      flake.metadata.neovimFeatures = inputs.nixpkgs.lib.concat [
        "rust"
        "zig"
      ];
    };
}
