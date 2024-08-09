{
  system ? builtins.currentSystem,
}:

let
  get-flake = import (
    builtins.fetchTarball "https://api.github.com/repos/ursi/get-flake/tarball/ac54750e3b95dab6ec0726d77f440efe6045bec1"
  );

  super = get-flake ../../.;
  inherit (super.inputs) nixpkgs fenix;

  pkgs = import nixpkgs { inherit system; };
  fenix' = fenix.packages.${system};
  toolchain = import ./toolchain.nix { inherit fenix'; };
  inherit (fenix'.default) rustfmt; # rustfmt nightly

  neovim = super.neovim.${system} [ super.inputs.neovim.modules.rust ];
in

pkgs.mkShell {
  packages = [
    toolchain
    rustfmt
    neovim
  ];
}
