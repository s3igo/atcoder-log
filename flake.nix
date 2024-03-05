{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    dotfiles.url = "github:s3igo/dotfiles";
  };

  outputs =
    {
      nixpkgs,
      flake-utils,
      dotfiles,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        tasks = import ./tasks.nix { inherit nixpkgs system; };
      in
      {
        packages = {
          neovim = dotfiles.neovim.${system} {
            inherit pkgs;
            modules = with dotfiles.nixosModules; [
              nix
              im-select
              {
                plugins = {
                  treesitter.grammarPackages = with pkgs.vimPlugins.nvim-treesitter.builtGrammars; [
                    rust
                    toml
                    regex
                  ];
                  lsp.servers.rust-analyzer = {
                    enable = true;
                    installCargo = false;
                    installRustc = false;
                    settings = {
                      check.command = "clippy";
                      files.excludeDirs = [ ".direnv" ];
                    };
                  };
                };
              }
            ];
          };
        };

        devShells.default = pkgs.mkShell { buildInputs = tasks; };
      }
    );
}
