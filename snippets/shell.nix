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

  inherit (container.packages.${system}) toolchain rustfmt-config;
  cargo-snippet = import ./cargo-snippet { inherit pkgs; };
  neovim = parent.neovim.${system} [
    parent.inputs.dotfiles.nixosModules.rust
    {
      plugins.lsp.servers.rust-analyzer.settings.rustfmt.extraArgs = [
        "--config-path"
        "${rustfmt-config}/rustfmt.toml"
      ];
    }
  ];
in

pkgs.mkShell {
  buildInputs = [
    toolchain
    cargo-snippet
    neovim
  ];
}
