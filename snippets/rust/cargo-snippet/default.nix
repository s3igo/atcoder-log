{
  lib,
  rustPlatform,
  fetchFromGitHub,
}:

rustPlatform.buildRustPackage rec {
  pname = "cargo-snippet";
  version = "0.6.5";

  src = fetchFromGitHub {
    owner = "hatoo";
    repo = "cargo-snippet";
    rev = "v${version}";
    hash = "sha256-70UETnz7TOTjWoxKrYXq/9WskKJnC65hLWtzdZnc1nE=";
  };

  cargoLock.lockFile = ./Cargo.lock;

  postPatch = ''
    ln -s ${./Cargo.lock} Cargo.lock
  '';

  doCheck = false;

  buildFeatures = [ "binaries" ];

  meta = {
    description = "A snippet extrator for competitive programmers";
    homepage = "https://github.com/hatoo/cargo-snippet";
    license = lib.licenses.mit;
    maintainers = [ ];
    mainProgram = "cargo-snippet";
  };
}
