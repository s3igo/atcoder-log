{
  description = "AtCoder development environment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      fenix,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        toolchain =
          with fenix.packages.${system};
          combine [
            (fromToolchainFile {
              file = ./contests/1.70.0/rust-toolchain.toml;
              sha256 = "sha256-gdYqng0y9iHYzYPAdkC/ka3DRny3La/S5G8ASj0Ayyc=";
            })
            # nightly versions of rust-analyzer and rustfmt
            rust-analyzer
            default.rustfmt
          ];
        cargo-compete = import ./cargo-compete.nix { inherit pkgs; };
        cargo-snippet = import ./cargo-snippet.nix { inherit pkgs; };
        tasks =
          let
            new = pkgs.writeScriptBin "task_new" ''
              cat <<EOF
              cargo compete new "$1" \
                  && git add "$1" \
                  && git commit -m "feat: add $1" \
                  && cd "$1"
              EOF
            '';
          in
          [ new ];
      in
      {
        devShell = pkgs.mkShell {
          buildInputs =
            let
              deps = [
                toolchain
                cargo-compete
                cargo-snippet
              ];
            in
            with pkgs;
            [
              statix
            ]
            ++ deps
            ++ tasks;
        };

        formatter = pkgs.nixfmt-rfc-style;
      }
    );
}
