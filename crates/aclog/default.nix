{ inputs, ... }:

{
  flake.neovimModules.aclog = with inputs.neovim-config.nixosModules; [
    default
    rust
  ];

  perSystem =
    {
      config,
      pkgs,
      inputs',
      ...
    }:

    let
      toolchain =
        with inputs'.fenix.packages;
        combine [
          (fromToolchainFile {
            file = ./rust-toolchain.toml;
            sha256 = "sha256-yMuSb5eQPO/bHv+Bcf/US8LVMbf/G/0MSfiPwBhiPpk=";
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
    in

    {
      packages.aclog = craneLib.buildPackage (
        commonArgs
        // {
          inherit cargoArtifacts;
          doCheck = false;
        }
      );

      checks = {
        aclog-build = config.packages.aclog;
        aclog-clippy = craneLib.cargoClippy (commonArgs // { inherit cargoArtifacts; });
        aclog-fmt = craneLib.cargoFmt { inherit src; };
        aclog-nextest = craneLib.cargoNextest (commonArgs // { inherit cargoArtifacts; });
      };

      devShells.aclog = pkgs.mkShellNoCC {
        packages = [
          toolchain
          (inputs.neovim-config.lib.customName {
            inherit pkgs;
            nvim = inputs'.nixvim.legacyPackages.makeNixvim {
              imports = config.flake.neovimModules.aclog;
            };
          })
        ];
      };
    };
}
