{ inputs, ... }:

{
  perSystem =
    {
      config,
      pkgs,
      inputs',
      ...
    }:

    let
      tasks =
        let
          test = pkgs.writeShellApplication {
            name = "t";
            runtimeInputs = [
              pkgs.time
              pkgs.online-judge-tools
              config.packages.rust-toolchain
            ];
            text = ''
              cargo build --release && oj test --command './target/release/main'
            '';
          };
          submit = pkgs.writeShellApplication {
            name = "s";
            runtimeInputs = [ pkgs.online-judge-tools ];
            text = ''
              oj submit --no-open --yes -- "$URL" ./src/main.rs
            '';
          };
          testAndSubmit = pkgs.writeShellApplication {
            name = "ts";
            runtimeInputs = [
              test
              submit
            ];
            text = ''
              t && s
            '';
          };
          run = pkgs.writeShellApplication {
            name = "r";
            runtimeInputs = [ config.packages.rust-toolchain ];
            text = ''
              cargo run --release
            '';
          };
        in
        [
          test
          submit
          testAndSubmit
          run
        ];
    in

    {
      packages = {
        rust-toolchain =
          with inputs'.fenix.packages;
          combine [
            (fromToolchainFile {
              file = ./rust-toolchain.toml;
              sha256 = "sha256-gdYqng0y9iHYzYPAdkC/ka3DRny3La/S5G8ASj0Ayyc=";
            })
            default.rustfmt # rustfmt nightly
          ];
        rustfmt-config = pkgs.stdenvNoCC.mkDerivation {
          name = "rustfmt-config";
          src = ./rustfmt.toml;
          phases = [ "installPhase" ];
          installPhase = ''
            mkdir -p $out/share
            cp $src $out/share/rustfmt.toml
          '';
        };
      };

      devShells.rust = pkgs.mkShell {
        packages = [
          config.packages.rust-toolchain
          (inputs'.nixvim.legacyPackages.makeNixvim {
            imports = with inputs.neovim-config.nixosModules; [
              default
              rust
              {
                plugins.lsp.servers.rust_analyzer.package = config.packages.rust-toolchain;
              }
            ];
          })
        ] ++ tasks;
        shellHook = ''
          export RUST_BACKTRACE=1
        '';
      };
    };
}
