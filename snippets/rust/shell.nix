{
  system ? builtins.currentSystem,
}:

let
  get-flake = import (
    builtins.fetchTarball "https://api.github.com/repos/ursi/get-flake/tarball/ac54750e3b95dab6ec0726d77f440efe6045bec1"
  );

  super = get-flake ../../.;
  container = get-flake ../../languages/rust;
  pkgs = import super.inputs.nixpkgs { inherit system; };

  inherit (container.packages.${system}) toolchain rustfmt-config;
  cargo-snippet = import ./cargo-snippet { inherit pkgs; };
  build = pkgs.writeShellApplication {
    name = "task_build";
    runtimeInputs = [
      pkgs.jq
      toolchain
      cargo-snippet
    ];
    text = ''
      PROJ_ROOT=$(git rev-parse --show-toplevel)

      cargo test --lib \
        && jq -s add rust.json <(cargo snippet -t vscode) \
          > "$PROJ_ROOT/languages/rust/rust.code-snippets"
    '';
  };
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
    build
    neovim
  ];
}
