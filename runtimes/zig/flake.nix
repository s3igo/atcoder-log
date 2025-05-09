{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    systems.url = "github:nix-systems/default";
  };

  outputs =
    inputs:

    let
      eachSystem = inputs.nixpkgs.lib.genAttrs (import inputs.systems);
      overlays = [
        (final: prev: {
          # Rename the executable as 'oj' expects 'gtime'
          # Using makeWrapper to avoid triggering package rebuilds
          time = prev.runCommand "time-wrapped" { nativeBuildInputs = [ prev.makeWrapper ]; } ''
            mkdir -p $out/bin
            makeWrapper ${prev.lib.getExe prev.time} $out/bin/gtime \
              --argv0 gtime
          '';
        })
      ];
      eachSystem' = f: eachSystem (system: f (import inputs.nixpkgs { inherit system overlays; }));
    in

    {
      packages = eachSystem' (pkgs: {
        zlint = pkgs.callPackage ./zlint { };
      });

      devShells = eachSystem' (pkgs: {
        default = pkgs.mkShellNoCC {
          packages = [
            pkgs.gitMinimal
            pkgs.just
            pkgs.online-judge-tools
            pkgs.time
            pkgs.zig
            pkgs.zls
            inputs.self.packages.${pkgs.system}.zlint
          ];
        };
      });

      metadata.neovimFeatures = inputs.nixpkgs.lib.concat [ "zig" ];
    };
}
