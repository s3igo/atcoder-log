{
  pkgs,
  fenix',
  crane,
}:

let
  toolchain = import ./toolchain.nix { inherit fenix'; };
  craneLib = (crane.mkLib pkgs).overrideToolchain toolchain;
  src = craneLib.cleanCargoSource ./.;
  commonArgs = {
    inherit src;
    strictDeps = true;
  };
  cargoArtifacts = craneLib.buildDepsOnly commonArgs;
  package = craneLib.buildPackage (
    commonArgs
    // {
      inherit cargoArtifacts;
      doCheck = false;
    }
  );
in

{
  checks = {
    aclog = package;
    aclog-clippy = craneLib.cargoClippy (commonArgs // { inherit cargoArtifacts; });
    aclog-fmt = craneLib.cargoFmt {
      inherit src;
      buildInputs = [ fenix'.default.rustfmt ];
    };
    aclog-nextest = craneLib.cargoNextest (commonArgs // { inherit cargoArtifacts; });
  };

  inherit package;
}
