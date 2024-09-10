{
  system ? builtins.currentSystem,
}:

let
  get-flake = import (
    fetchTarball "https://api.github.com/repos/ursi/get-flake/tarball/ac54750e3b95dab6ec0726d77f440efe6045bec1"
  );
  super = get-flake ../../.;
  inherit (super.inputs) nixpkgs nixvim neovim-config;
  pkgs = import nixpkgs { inherit system; };
  pkgs-nim_1_6_14 =
    import
      (builtins.fetchTarball "https://api.github.com/repos/nixos/nixpkgs/tarball/a71323f68d4377d12c04a5410e214495ec598d4c")
      { inherit system; };

  nativeBuildInputs = [
    pkgs-nim_1_6_14.nim
    pkgs-nim_1_6_14.nimPackages.nimble
  ];

  neovim = nixvim.legacyPackages.${system}.makeNixvim {
    imports = [
      neovim-config.nixosModules.default
      {
        autoCmd = [
          {
            event = "FileType";
            pattern = "nim";
            command = "setlocal shiftwidth=2";
          }
        ];

        plugins.none-ls = {
          enable = true;
          sources.formatting.nimpretty.enable = true;
        };

        extraPackages = [
          pkgs-nim_1_6_14.nim
          pkgs.nimlangserver
        ];

        extraConfigLuaPost = ''
          require('lspconfig').nim_langserver.setup({
            settings = {
              nim = { nimsuggestPath = "${pkgs-nim_1_6_14.nim}/bin/nimsuggest" }
            }
          })
        '';
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
          nimble build && oj test --command './main'
        '';
      };
      submit = pkgs.writeShellApplication {
        name = "s";
        runtimeInputs = [ pkgs.online-judge-tools ];
        text = ''
          oj submit --no-open --yes -- "$URL" ./main.nim
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
          nimble run
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
  # shellHook = ''
  #   nimble setup
  # '';
}
