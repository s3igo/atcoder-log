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
      sdk = "dotnet-sdk_9";
    in

    {
      packages = eachSystem' (pkgs: {
        fsharplint = pkgs.callPackage ./fsharplint.nix { };
      });

      devShells = eachSystem' (pkgs: {
        default = pkgs.mkShellNoCC {
          packages = [
            pkgs.gitMinimal
            pkgs.just
            pkgs.online-judge-tools
            pkgs.${sdk}
            pkgs.fantomas
            pkgs.fsautocomplete
            inputs.self.packages.${pkgs.system}.fsharplint
            (pkgs.writeShellScriptBin "j" ''
              exec ${pkgs.lib.getExe pkgs.just} "$@"
            '')
            # Rename the executable as 'oj' expects 'gtime'
            (pkgs.writeShellScriptBin "gtime" ''
              exec ${pkgs.lib.getExe pkgs.time} "$@"
            '')
          ];
          # https://learn.microsoft.com/en-us/dotnet/core/extensions/globalization-icu
          shellHook = with pkgs; ''
            export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:${lib.makeLibraryPath [ icu ]}";
            dotnet sdk check
          '';
          DOTNET_ROOT = pkgs.${sdk} + "/share/dotnet";
        };
      });

      metadata.neovimFeatures = inputs.nixpkgs.lib.concat [
        "fsharp"
        {
          autoCmd = [
            {
              event = "FileType";
              pattern = "fsharp";
              callback.__raw = ''
                function()
                  vim.opt_local.commentstring = '// %s'
                end
              '';
            }
          ];
        }
      ];
    };
}
