{ pkgs }:
pkgs.rustPlatform.buildRustPackage rec {
  pname = "cargo-compete";
  version = "0.10.6";
  src = pkgs.fetchFromGitHub {
    owner = "qryxip";
    repo = pname;
    rev = version;
    hash = "sha256-trtnxWDXzCeZ7ICLbPgCrBFZZzOmpkGOjjrpus6t+is=";
  };
  cargoHash = "sha256-A8DAsbQDu9I8vEuDxBszADm45Q8NjnMDO8mD+ADl224=";

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
