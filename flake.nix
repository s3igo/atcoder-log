{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    crane = {
      url = "github:ipetkov/crane";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    neovim.url = "github:s3igo/dotfiles?dir=neovim";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      fenix,
      crane,
      neovim,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        tasks = import ./tasks.nix { inherit nixpkgs system; };
        fenix' = fenix.packages.${system};
        procon-log = import ./crates/procon-log { inherit pkgs fenix' crane; };
        procon-log' = procon-log.package;
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

        inherit (procon-log) checks;

        apps = {
          procon-log = {
            type = "app";
            program = "${procon-log'}/bin/log";
          };
          default = self.apps.${system}.procon-log;
        };

        packages = {
          neovim = self.neovim.${system} (with neovim.modules; [ markdown ]);
          procon-log = procon-log';
          default = procon-log';
        };

        devShells.default = pkgs.mkShell {
          packages =
            let
              inherit (self.packages.${system}) neovim procon-log;
            in
            [
              neovim
              procon-log
            ]
            ++ (with pkgs; [ online-judge-tools ])
            ++ tasks;
        };
      }
    );
}
