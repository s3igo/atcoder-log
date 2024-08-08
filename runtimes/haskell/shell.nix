{
  system ? builtins.currentSystem,
}:

let
  get-flake = import (
    fetchTarball "https://api.github.com/repos/ursi/get-flake/tarball/ac54750e3b95dab6ec0726d77f440efe6045bec1"
  );
  super = get-flake ../../.;
  inherit (super) neovim;
  pkgs = import super.inputs.nixpkgs { inherit system; };
  pkgs-cabal-install_3_8_1_0 =
    import
      (builtins.fetchTarball "https://api.github.com/repos/nixos/nixpkgs/tarball/98f3b08f58ff125ef02d55cd52a83f44f245f2ea")
      { inherit system; };
  buildInputs = with pkgs; [ llvm_14 ];
in

pkgs.mkShell {
  inherit buildInputs;
  packages = [
    pkgs.haskell.compiler.ghc945 # ghc 9.4.5
    pkgs-cabal-install_3_8_1_0.cabal-install # cabal-install 3.8.1.0
  ];
}
