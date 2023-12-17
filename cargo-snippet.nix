{ nixpkgs, system, rust-overlay }:
let
  overlays = [
    (import rust-overlay)
    (self: super: {
      # Cargo.toml was last updated on 2020-07-19
      rustToolchain = super.rust-bin.nightly."2021-01-01".default;
     })
  ];
  pkgs = import nixpkgs { inherit system overlays; };
in
pkgs.rustPlatform.buildRustPackage rec {
  pname = "cargo-snippet";
  version = "0.1.2";

  src = pkgs.fetchFromGitHub {
    owner = "hatoo";
    repo = "cargo-snippet";
    rev = version;
    hash = "sha256-lSsLQnK1XBQDx00C2LULzeD1R5MZGwEONgvLfP7igt0=";
  };

  nativeBuildInputs = [ pkgs.rustToolchain ];

  cargoBuildFlags = [
    "--offline"
    "-Z"
    "unstable-options"
  ];
  # buildNoDefaultFeatures = false;
  buildFeatures = [
    "binaries"
    # "const_fn"
    # "thread_local_state"
    ];
  # HACK: see https://github.com/NixOS/nixpkgs/issues/261412
  postConfigure = ''
    cargo metadata --offline
  '';

  cargoHash = "sha256-1+esdZhaBQI/EW9vzw5ZSCSdA9DASpBY0O1n9OmMb7c=";
}

# wip
