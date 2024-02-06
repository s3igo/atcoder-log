{ pkgs }:
pkgs.rustPlatform.buildRustPackage rec {
  pname = "cargo-compete";
  version = "0.10.6";
  src = pkgs.fetchFromGitHub {
    owner = "qryxip";
    repo = pname;
    rev = "f17c37417b270c8a726eab8ee5a6ea09a202348a";
    hash = "sha256-OgT8jwpRmwBrwtzue3pDwjNkzJmzHzpNTi889ucs8wY=";
  };
  cargoHash = "sha256-V3lA9IlAWvWS+EHF10XNylALBe1Jh8FFwGPJPCemIGM=";

  nativeBuildInputs = [ pkgs.pkg-config ];

  buildInputs =
    with pkgs;
    [
      libgit2
      openssl
      zlib
    ]
    ++ lib.optionals stdenv.isDarwin [ darwin.apple_sdk.frameworks.Security ];

  doCheck = false;
}
