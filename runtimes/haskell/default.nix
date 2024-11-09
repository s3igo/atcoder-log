{ inputs, ... }:

{
  perSystem =
    {
      pkgs,
      inputs',
      system,
      ...
    }:

    let
      pkgs-cabal-install_3_8_1_0 = import (builtins.fetchTarball {
        url = "https://api.github.com/repos/nixos/nixpkgs/tarball/98f3b08f58ff125ef02d55cd52a83f44f245f2ea";
        sha256 = "sha256:03w39dd43jgi09j7dz0kj17jwa78xig28ix3b58mpa8hvaj4bzqs";
      }) { inherit system; };
      pkgs-haskell-language-server_2_2_0_0 = import (builtins.fetchTarball {
        url = "https://api.github.com/repos/nixos/nixpkgs/tarball/a2eb207f45e4a14a1e3019d9e3863d1e208e2295";
        sha256 = "sha256:1128jz9mmpgx2f84xm0r1jhd1d742zwsh4alkgra0855y27531gp";
      }) { inherit system; };

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

      buildInputs = [ pkgs.llvm_14 ];

      neovim = inputs'.nixvim.legacyPackages.makeNixvim {
        imports = [
          inputs.neovim-config.nixosModules.default
          {
            autoCmd = [
              {
                event = "FileType";
                pattern = "haskell";
                command = "setlocal shiftwidth=2";
              }
            ];
            plugins.lsp.servers.hls = {
              enable = true;
              package = haskell-language-server;
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

    {
      devShells.haskell = pkgs.mkShellNoCC {
        inherit buildInputs;
        packages = [
          pkgs.time
          cabal-install
          ghc
          neovim
        ] ++ tasks;
      };
    };
}
