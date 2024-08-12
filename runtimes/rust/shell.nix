{
  system ? builtins.currentSystem,
}:

let
  get-flake = import (
    fetchTarball "https://api.github.com/repos/ursi/get-flake/tarball/ac54750e3b95dab6ec0726d77f440efe6045bec1"
  );
  super = get-flake ../../.;
  inherit (super.inputs) nixpkgs fenix neovim;
  pkgs = import nixpkgs { inherit system; };

  toolchain =
    with fenix.packages.${system};
    combine [
      (fromToolchainFile {
        file = ./rust-toolchain.toml;
        sha256 = "sha256-gdYqng0y9iHYzYPAdkC/ka3DRny3La/S5G8ASj0Ayyc=";
      })
      default.rustfmt # rustfmt nightly
    ];
  neovim' = neovim.withModules {
    inherit system pkgs;
    modules = with neovim.modules; [
      im-select
      rust
    ];
  };
in

pkgs.mkShell {
  packages = [
    toolchain
    neovim'
  ];
}
