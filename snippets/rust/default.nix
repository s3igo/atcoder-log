{ inputs, ... }:

{
  perSystem =
    {
      config,
      pkgs,
      lib,
      inputs',
      ...
    }:

    let
      nvim = inputs'.nixvim.legacyPackages.makeNixvim {
        imports = with inputs.neovim-config.nixosModules; [
          default
          nix
          rust
          {
            plugins.lsp.servers.rust_analyzer = {
              package = config.packages.rust-toolchain;
              settings.rustfmt.extraArgs = [
                "--config-path"
                "${config.packages.rustfmt-config}/share/rustfmt.toml"
              ];
            };
          }
        ];
      };
      build = pkgs.writeShellApplication {
        name = "build";
        runtimeInputs = [
          pkgs.jq
          config.packages.rust-toolchain
          config.packages.cargo-snippet
        ];
        text = ''
          cargo test --lib \
            && jq -s add rust.json <(cargo snippet -t vscode) \
              > "$(${lib.getExe config.flake-root.package})"/languages/rust/rust.code-snippets
        '';
      };
    in

    {
      packages.cargo-snippet = pkgs.callPackage ./cargo-snippet { };

      devShells.snippets-rust = pkgs.mkShell {
        packages = [
          config.packages.rust-toolchain
          config.packages.cargo-snippet
          pkgs.cargo-nextest
          (inputs.neovim-config.lib.customName { inherit pkgs nvim; })
          build
        ];
        shellHook = ''
          export RUST_BACKTRACE=1
        '';
      };
    };
}
