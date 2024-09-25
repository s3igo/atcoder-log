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
        aclog = import ./crates/procon-log { inherit pkgs fenix' crane; };
      in
      {
        inherit (aclog) checks;

        packages = {
          neovim = nixvim.legacyPackages.${system}.makeNixvim {
            imports = with neovim-config.nixosModules; [
              default
              nix
              markdown
            ];
          };
          aclog = aclog.package;
          default = aclog.package;
        };

        devShells.default = pkgs.mkShell {
          packages =
            let
              inherit (self.packages.${system}) neovim aclog;
            in
            [
              neovim
              aclog
            ]
            ++ (with pkgs; [ online-judge-tools ])
            ++ tasks;
        };
      }
    );
}
