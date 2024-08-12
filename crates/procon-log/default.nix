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
  procon-log = craneLib.buildPackage (
    commonArgs
    // {
      inherit cargoArtifacts;
      doCheck = false;
    }
  );
in

{
  checks = {
    inherit procon-log;
    procon-log-clippy = craneLib.cargoClippy (commonArgs // { inherit cargoArtifacts; });
    procon-log-fmt = craneLib.cargoFmt {
      inherit src;
      buildInputs = [ fenix'.default.rustfmt ];
    };
    procon-log-nextest = craneLib.cargoNextest (commonArgs // { inherit cargoArtifacts; });
  };

  package = procon-log;
}
