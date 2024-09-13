{
  system ? builtins.currentSystem,
}:

let
  get-flake = import (
    fetchTarball "https://api.github.com/repos/ursi/get-flake/tarball/ac54750e3b95dab6ec0726d77f440efe6045bec1"
  );
  super = get-flake ../../.;
  inherit (super.inputs) nixpkgs nixvim neovim-config;

  overlays = [ (import ../overlay.nix) ];
  pkgs = import nixpkgs { inherit system overlays; };

  nativeBuildInputs = with pkgs; [
    crystal_1_9
    shards
  ];

  neovim = nixvim.legacyPackages.${system}.makeNixvim {
    imports = [
      neovim-config.nixosModules.default
      {
        autoCmd = [
          {
            event = "FileType";
            pattern = "crystal";
            command = "setlocal shiftwidth=2";
          }
        ];

        plugins.none-ls = {
          enable = true;
          sources.formatting.crystal_format.enable = true;
        };
        # crystaline
      }
    ];
  };
  tasks =
    let
      test = pkgs.writeShellApplication {
        name = "t";
        runtimeInputs =
          with pkgs;
          [
            time
            online-judge-tools
          ]
          ++ nativeBuildInputs;
        text = ''
          shards build --release --no-debug --no-color && oj test --command './bin/main'
        '';
      };
      submit = pkgs.writeShellApplication {
        name = "s";
        runtimeInputs = [ pkgs.online-judge-tools ];
        text = ''
          oj submit --no-open --yes -- "$URL" ./main.cr
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
        runtimeInputs = nativeBuildInputs;
        text = ''
          shards run --release --no-debug --no-color
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
  inherit nativeBuildInputs;
  packages = [ neovim ] ++ tasks;
}
