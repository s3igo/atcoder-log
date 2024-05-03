{ pkgs }:

pkgs.rustPlatform.buildRustPackage rec {
  pname = "cargo-snippet";
  version = "0.6.5";

  src = pkgs.fetchFromGitHub {
    owner = "hatoo";
    repo = pname;
    rev = "v${version}";
    hash = "sha256-70UETnz7TOTjWoxKrYXq/9WskKJnC65hLWtzdZnc1nE=";
  };

  cargoHash = "sha256-z5VOo6nCDK5IZcKY0qocjcXeX0dKeTrr+i13jCqHm8c=";

  cargoPatches = [ ./add-lockfile.patch ];

  doCheck = false;

  buildFeatures = [ "binaries" ];

  meta = with pkgs.lib; {
    description = "A snippet extrator for competitive programmers";
    homepage = "https://github.com/hatoo/cargo-snippet";
    license = licenses.mit;
    maintainers = [ ];
    mainProgram = "cargo-snippet";
  };
}
