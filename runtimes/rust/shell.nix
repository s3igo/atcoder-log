{
  system ? builtins.currentSystem,
}:

let
  get-flake = import (
    fetchTarball "https://api.github.com/repos/ursi/get-flake/tarball/ac54750e3b95dab6ec0726d77f440efe6045bec1"
  );
  super = get-flake ../../.;
  inherit (super.inputs)
    nixpkgs
    fenix
    nixvim
    neovim-config
    ;
  overlays = [ super.outputs.overlays.time ];
  pkgs = import nixpkgs { inherit system overlays; };

  toolchain =
    with fenix.packages.${system};
    combine [
      (fromToolchainFile {
        file = ./rust-toolchain.toml;
        sha256 = "sha256-gdYqng0y9iHYzYPAdkC/ka3DRny3La/S5G8ASj0Ayyc=";
      })
      default.rustfmt # rustfmt nightly
    ];
  neovim = nixvim.legacyPackages.${system}.makeNixvim {
    imports =
      with neovim-config.nixosModules;
      [
        default
        rust
      ]
      ++ [
        {
          plugins.lsp.servers.rust-analyzer.package = toolchain;
        }
      ];
  };
  tasks =
    let
      test = pkgs.writeShellApplication {
        name = "t";
        runtimeInputs = [
          pkgs.time
          pkgs.online-judge-tools
          toolchain
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
        runtimeInputs = [ toolchain ];
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

pkgs.mkShell {
  packages = [
    toolchain
    neovim
  ] ++ tasks;
}
