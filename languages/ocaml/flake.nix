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
        ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_0.overrideScope' (
          self: super: { ocaml = super.ocaml.override { flambdaSupport = true; }; }
        );
        tasks = import ./tasks.nix { inherit pkgs; };
      in
      {
        packages.neovim = neovim.withModules {
          inherit system pkgs;
          grammars = [ "ocaml" ];
          modules = with neovim.nixosModules; [
            nix
            {
              autoCmd = [
                {
                  event = "FileType";
                  pattern = "ocaml";
                  command = "setlocal shiftwidth=2";
                }
              ];
              plugins.lsp.servers.ocamllsp = {
                enable = true;
                package = ocamlPackages.ocaml-lsp;
                rootDir = "function() return '/workspace' end";
                cmd = [
                  "ocamllsp"
                  "--fallback-read-dot-merlin"
                ];
              };
            }
          ];
        };

        devShells.default = pkgs.mkShell {
          buildInputs =
            [ self.packages.${system}.neovim ]
            ++ (with pkgs; [
              online-judge-tools
              fish
            ])
            ++ (with ocamlPackages; [
              # for compiling
              ocaml
              findlib
              # for ocamllsp
              dot-merlin-reader
              ocamlformat_0_26_1
              # deps
              num
              containers
              core
              iter
              batteries
            ])
            ++ tasks;
        };
      }
    );
}
