{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    systems.url = "github:nix-systems/default";
    flake-root.url = "github:srid/flake-root";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    crane.url = "github:ipetkov/crane";
    nixvim = {
      url = "github:nix-community/nixvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    neovim-config.url = "github:s3igo/dotfiles?dir=neovim-config";
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
        ./snippets/rust
      ];

      systems = import inputs.systems;

      perSystem =
        {
          config,
          pkgs,
          inputs',
          system,
          ...
        }:

        {
          _module.args.pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = [
              (final: prev: {
                time = prev.time.overrideAttrs {
                  # Rename the binary from 'time' to 'gtime'
                  postInstall = ''
                    mv $out/bin/time $out/bin/gtime
                  '';
                };
              })
            ];
          };

          packages = {
            default = config.packages.aclog;
            neovim = inputs'.nixvim.legacyPackages.makeNixvim {
              imports = with inputs.neovim-config.nixosModules; [
                default
                nix
                markdown
              ];
            };
          };

          devShells.default = pkgs.mkShellNoCC {
            packages = [
              pkgs.online-judge-tools
              config.packages.aclog
              (inputs.neovim-config.lib.customName {
                inherit pkgs;
                nvim = config.packages.neovim;
              })
            ];
          };
        };
    };
}
