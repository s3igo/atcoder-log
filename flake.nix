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
    nixvim = {
      url = "github:nix-community/nixvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    neovim-config.url = "github:s3igo/dotfiles?dir=neovim-config";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      fenix,
      crane,
      nixvim,
      neovim-config,
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
        inherit (procon-log) checks;

        apps = {
          procon-log = {
            type = "app";
            program = "${procon-log'}/bin/aclog";
          };
          default = self.apps.${system}.procon-log;
        };

        packages = {
          neovim = nixvim.legacyPackages.${system}.makeNixvim {
            imports = with neovim-config.nixosModules; [
              default
              nix
              markdown
            ];
          };
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
