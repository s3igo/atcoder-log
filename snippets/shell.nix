{
  system ? builtins.currentSystem,
}:

let
  get-flake = import (
    builtins.fetchTarball "https://api.github.com/repos/ursi/get-flake/tarball/ac54750e3b95dab6ec0726d77f440efe6045bec1"
  );

  super = get-flake ../.;
  container = get-flake ../languages/rust;
  pkgs = import super.inputs.nixpkgs { inherit system; };

  inherit (container.packages.${system}) toolchain rustfmt-config;
  cargo-snippet = import ./cargo-snippet { inherit pkgs; };
  neovim = super.neovim.${system} [
    super.inputs.neovim.nixosModules.rust
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
