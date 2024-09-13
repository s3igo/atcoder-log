{
  system ? builtins.currentSystem,
}:

let
  get-flake = import (
    fetchTarball "https://api.github.com/repos/ursi/get-flake/tarball/ac54750e3b95dab6ec0726d77f440efe6045bec1"
  );
  super = get-flake ../../.;
  inherit (super.inputs) nixpkgs nixvim neovim-config;

  overlays = [ (import ../overlay.nix) ];
  pkgs = import nixpkgs { inherit system overlays; };
  pkgs-gmp_6_2_1 =
    import
      (builtins.fetchTarball "https://api.github.com/repos/nixos/nixpkgs/tarball/0b07d4957ee1bd7fd3bdfd12db5f361bd70175a6")
      { inherit system; };
  pkgs-eigen_3_4_0 =
    import
      (builtins.fetchTarball "https://api.github.com/repos/nixos/nixpkgs/tarball/5629520edecb69630a3f4d17d3d33fc96c13f6fe")
      { inherit system; };
  pkgs-z3_4_8_12 =
    import
      (builtins.fetchTarball "https://api.github.com/repos/nixos/nixpkgs/tarball/6f05cfdb1e78d36c0337516df674560e4b51c79b")
      { inherit system; };

  ac-library =
    with pkgs;
    stdenv.mkDerivation rec {
      pname = "ac-library";
      version = "1.5.1";
      src = fetchurl {
        url = "https://github.com/atcoder/ac-library/releases/download/v${version}/ac-library.zip";
        sha256 = "sha256-bcC+zrumm7UjzpB6MZZv4w05CYk17GtLUonJTkJjS+E=";
      };
      nativeBuildInputs = [ unzip ];
      unpackPhase = ''
        mkdir -p $out/lib
        unzip $src -d $out/lib
      '';
    };

  nativeBuildInputs = with pkgs; [
    pkg-config
    clang_16
    lld
  ];
  buildInputs = [
    ac-library
    pkgs.boost182
    pkgs-gmp_6_2_1.gmp6
    pkgs-eigen_3_4_0.eigen
    pkgs-z3_4_8_12.z3
  ];

  neovim = nixvim.legacyPackages.${system}.makeNixvim {
    imports = [
      neovim-config.nixosModules.default
      {
        plugins = {
          lsp.servers.clangd = {
            enable = true;
            onAttach.function = ''
              require("clangd_extensions.inlay_hints").setup_autocmd()
              require("clangd_extensions.inlay_hints").set_inlay_hints()
            '';
          };
          clangd-extensions = {
            enable = true;
            enableOffsetEncodingWorkaround = true;
          };
        };
      }
    ];
  };
  tasks =
    let
      test = pkgs.writeShellApplication {
        name = "t";
        runtimeInputs =
          with pkgs;
          [
            time
            online-judge-tools
          ]
          ++ nativeBuildInputs
          ++ buildInputs;
        text = ''
          clang++ -std=c++2b -Wall -Wextra -O2 -mtune=native -march=native -fconstexpr-depth=2147483647 \
            "$(pkg-config --cflags --libs boost gmpxx eigen3)" -I${ac-library}/lib \
            -fuse-ld=lld -o ./a.out ./main.cpp \
            && oj test --command './a.out'
        '';
      };
      submit = pkgs.writeShellApplication {
        name = "s";
        runtimeInputs = [ pkgs.online-judge-tools ];
        text = ''
          oj submit --no-open --yes --guess-cxx-compiler clang -- "$URL" ./main.cpp
        '';
      };
      testAndSubmit = pkgs.writeShellApplication {
        name = "ts";
        runtimeInputs = [
          test
          submit
        ];
        text = ''
          t && s
        '';
      };
      run = pkgs.writeShellApplication {
        name = "r";
        runtimeInputs = nativeBuildInputs ++ buildInputs;
        text = ''
          clang++ -std=c++2b -Wall -Wextra -O2 -mtune=native -march=native -fconstexpr-depth=2147483647 \
            "$(pkg-config --cflags --libs boost gmpxx eigen3)" -I${ac-library}/lib \
            -fuse-ld=lld -o ./a.out ./main.cpp \
            && ./a.out
        '';
      };
    in
    [
      test
      submit
      testAndSubmit
      run
    ];
in

pkgs.mkShell {
  inherit nativeBuildInputs buildInputs;
  packages = [ neovim ] ++ tasks;
}
