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
    crane.url = "github:ipetkov/crane";
    advisory-db = {
      url = "github:rustsec/advisory-db";
      flake = false;
    };
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        ./crates/aclog
        ./runtimes/rust
        ./runtimes/haskell
        ./snippets/rust
      ];
      # ++ (map (path: ./runtimes/${path}) (with builtins; attrNames (readDir ./runtimes)));

      systems = import inputs.systems;

      perSystem =
        {
          pkgs,
          self',
          system,
          ...
        }:

        {
          _module.args.pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = [
              (final: prev: {
                time = prev.time.overrideAttrs {
                  # Renaming the binary because oj expects "gtime"
                  postInstall = ''
                    mv $out/bin/time $out/bin/gtime
                  '';
                };
              })
            ];
          };

          packages.default = self'.packages.aclog;

          devShells.default = pkgs.mkShell {
            inputsFrom = [ self'.devShells.aclog ];
            packages = with pkgs; [
              just
              nil
              nixd
              online-judge-tools
            ];
          };
        };

      flake.metadata.neovimFeatures = inputs.nixpkgs.lib.concat [ "rust" ];
    };
}
