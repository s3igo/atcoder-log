{
  system ? builtins.currentSystem,
}:

let
  get-flake = import (
    builtins.fetchTarball "https://api.github.com/repos/ursi/get-flake/tarball/ac54750e3b95dab6ec0726d77f440efe6045bec1"
  );
  parent = get-flake ../.;
  container = get-flake ../containers/rust;
  pkgs = import parent.inputs.nixpkgs { inherit system; };
in

pkgs.mkShell {
  buildInputs = [
    container.packages.${system}.toolchain
    (import ./cargo-snippet { inherit pkgs; })
  ];
}
