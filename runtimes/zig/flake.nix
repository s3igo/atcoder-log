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
            pkgs.wget
            pkgs.zig
            pkgs.zls
            inputs.self.packages.${pkgs.system}.zlint
            (pkgs.writeShellScriptBin "j" ''
              exec ${pkgs.lib.getExe pkgs.just} "$@"
            '')
            # Rename the executable as 'oj' expects 'gtime'
            (pkgs.writeShellScriptBin "gtime" ''
              exec ${pkgs.lib.getExe pkgs.time} "$@"
            '')
          ];
        };
      });

      metadata.neovimFeatures = inputs.nixpkgs.lib.concat [ "zig" ];
    };
}
