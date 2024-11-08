{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
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
    nixvim = {
      url = "github:nix-community/nixvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    neovim-config.url = "github:s3igo/dotfiles?dir=neovim-config";
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ ./crates/aclog ];

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
