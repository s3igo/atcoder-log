{
  inputs = {
    nixpkgs.url = "https://channels.nixos.org/nixos-unstable/nixexprs.tar.xz";
    systems.url = "github:nix-systems/default";
  };

  outputs =
    inputs:

    let
      eachSystem = inputs.nixpkgs.lib.genAttrs (import inputs.systems);
      eachSystem' = f: eachSystem (system: f (import inputs.nixpkgs { inherit system; }));
      ghc = "ghc984";
    in

    {
      devShells = eachSystem' (pkgs: {
        default = pkgs.mkShellNoCC {
          packages = with pkgs; [
            gitMinimal
            just
            online-judge-tools
            haskell.compiler.${ghc}
            haskell.packages.${ghc}.cabal-install
            haskell.packages.${ghc}.haskell-language-server
            haskell.packages.${ghc}.ormolu
            haskell.packages.${ghc}.hlint
            (writeShellScriptBin "j" ''
              exec ${lib.getExe just} "$@"
            '')
            # Rename the executable as 'oj' expects 'gtime'
            (writeShellScriptBin "gtime" ''
              exec ${lib.getExe time} "$@"
            '')
          ];
        };
      });

      metadata.neovimFeatures = inputs.nixpkgs.lib.concat [
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
            installGhc = false;
          };
        }
      ];
    };
}
