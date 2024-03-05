{
  description = "AtCoder development environment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
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
      nixvim,
      dotfiles,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
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

        devShells.default = pkgs.mkShell { buildInputs = tasks; };
      }
    );
}
