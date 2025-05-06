{ inputs, self, ... }:

{
  perSystem =
    {
      config,
      lib,
      pkgs,
      inputs',
      self',
      ...
    }:

    let
      toolchain =
        with inputs'.fenix.packages;
        combine [
          (fromToolchainFile {
            file = ./rust-toolchain.toml;
            sha256 = "sha256-X/4ZBHO3iW0fOenQ3foEvscgAPJYl2abspaBThDOukI=";
          })
          default.rustfmt # rustfmt nightly
        ];
      craneLib = (inputs.crane.mkLib pkgs).overrideToolchain toolchain;
      src = craneLib.cleanCargoSource ./.;
      commonArgs = {
        inherit src;
        strictDeps = true;
      };
      cargoArtifacts = craneLib.buildDepsOnly commonArgs;
      commonArgs' = commonArgs // {
        inherit cargoArtifacts;
      };
    in

    {
      packages = {
        _deps-aclog = cargoArtifacts;
        aclog = craneLib.buildPackage commonArgs';
      };

      checks = {
        aclog-build = craneLib.buildPackage (commonArgs' // { doCheck = false; });
        aclog-clippy = craneLib.cargoClippy (
          commonArgs' // { cargoClippyExtraArgs = "--all-targets -- -D warnings"; }
        );
        aclog-fmt = craneLib.cargoFmt { inherit src; };
        aclog-nextest = craneLib.cargoNextest commonArgs';
        aclog-audit = craneLib.cargoAudit {
          inherit src;
          inherit (inputs) advisory-db;
        };
      };

      devShells.aclog = craneLib.devShell {
        checks = lib.filterAttrs (name: _: builtins.match "^aclog-.*" name != null) self'.checks;
        packages = [
          toolchain
        ];
      };
    };
}
