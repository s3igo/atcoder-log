{
  system ? builtins.currentSystem,
}:

let
  get-flake = import (
    builtins.fetchTarball "https://api.github.com/repos/ursi/get-flake/tarball/ac54750e3b95dab6ec0726d77f440efe6045bec1"
  );

  super = get-flake ../../.;
  inherit (super.inputs)
    nixpkgs
    fenix
    nixvim
    neovim-config
    ;

  pkgs = import nixpkgs { inherit system; };
  fenix' = fenix.packages.${system};
  toolchain = import ./toolchain.nix { inherit fenix'; };
  inherit (fenix'.default) rustfmt; # rustfmt nightly

  neovim = nixvim.legacyPackages.${system}.makeNixvim {
    imports = with neovim-config.nixosModules; [
      default
      rust
    ];
  };
in

pkgs.mkShell {
  packages = [
    toolchain
    rustfmt
    neovim
  ];
}
