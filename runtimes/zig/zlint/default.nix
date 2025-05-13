{
  lib,
  stdenv,
  zig,
  fetchFromGitHub,
  callPackage,
}:

stdenv.mkDerivation rec {
  pname = "zlint";
  version = "0.7.7";

  src = fetchFromGitHub {
    owner = "DonIsaac";
    repo = "zlint";
    rev = "v${version}";
    hash = "sha256-FdTqPowA+6YKY24yw470d1vMdKVrcWUDHFjdBmwEXcg=";
  };

  nativeBuildInputs = [ zig.hook ];

  postPatch = ''
    ln -s ${callPackage ./deps.nix { }} $ZIG_GLOBAL_CACHE_DIR/p
  '';

  meta = {
    description = "A linter for the Zig programming language";
    homepage = "https://github.com/DonIsaac/zlint";
    license = lib.licenses.mit;
    maintainers = [ ];
    mainProgram = "zlint";
  };
}
