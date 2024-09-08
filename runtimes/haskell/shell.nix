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

  pkgs = import nixpkgs { inherit system; };
  pkgs-cabal-install_3_8_1_0 =
    import
      (builtins.fetchTarball "https://api.github.com/repos/nixos/nixpkgs/tarball/98f3b08f58ff125ef02d55cd52a83f44f245f2ea")
      { inherit system; };
  pkgs-haskell-language-server_2_2_0_0 =
    import
      (builtins.fetchTarball "https://api.github.com/repos/nixos/nixpkgs/tarball/a2eb207f45e4a14a1e3019d9e3863d1e208e2295")
      { inherit system; };

  # cabal-install 3.8.1.0
  inherit (pkgs-cabal-install_3_8_1_0) cabal-install;

  # ghc 9.4.5
  # Use pkgs-haskell-language-server_2_2_0_0 instead of pkgs
  # due to ABI don't match with haskell-language-server
  ghc = pkgs-haskell-language-server_2_2_0_0.haskell.compiler.ghc945;

  # For ghc 9.4.5
  # See: https://haskell-language-server.readthedocs.io/en/latest/support/ghc-version-support.html
  #      https://nixos.org/manual/nixpkgs/unstable/#haskell-language-server
  haskell-language-server = pkgs-haskell-language-server_2_2_0_0.haskell-language-server.override {
    supportedGhcVersions = [ "945" ];
  };

  buildInputs = with pkgs; [ llvm_14 ];

  neovim = nixvim.legacyPackages.${system}.makeNixvim {
    imports = [
      neovim-config.nixosModules.default
      {
        autoCmd = [
          {
            event = "FileType";
            pattern = "haskell";
            command = "setlocal shiftwidth=2";
          }
        ];
        plugins = {
          lsp.servers.hls = {
            enable = true;
            package = haskell-language-server;
          };
        };
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
          cabal-install
        ];
        text = ''
          cabal v2-build --offline && oj test --command "$(cabal list-bin main)"
        '';
      };
      submit = pkgs.writeShellApplication {
        name = "s";
        runtimeInputs = [ pkgs.online-judge-tools ];
        text = ''
          oj submit --no-open --yes -- "$URL" ./app/Main.hs
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
        runtimeInputs = [ cabal-install ];
        text = ''
          cabal v2-run --offline
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
  inherit buildInputs;
  packages = [
    pkgs.time
    cabal-install
    ghc
    neovim
  ] ++ tasks;
}
