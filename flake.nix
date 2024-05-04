{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    neovim.url = "github:s3igo/dotfiles?dir=neovim";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      neovim,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        tasks = import ./tasks.nix { inherit nixpkgs system; };
      in
      {
        neovim =
          modules:
          neovim.withModules {
            inherit system pkgs;
            modules =
              with neovim.modules;
              [
                im-select
                nix
              ]
              ++ modules;
          };

        packages.neovim = self.neovim.${system} (with neovim.modules; [ markdown ]);

        devShells.default = pkgs.mkShell { buildInputs = [ self.packages.${system}.neovim ] ++ tasks; };
      }
    );
}
