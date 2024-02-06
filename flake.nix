{
  description = "AtCoder development environment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      rust-overlay,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        overlays = [
          (import rust-overlay)
          (self: super: {
            rustToolchain = super.rust-bin.fromRustupToolchainFile ./contests/1.70.0/rust-toolchain.toml;
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; };
        cargo-compete = import ./cargo-compete.nix { inherit pkgs; };
        # cargo-snippet = import ./cargo-snippet.nix { inherit nixpkgs system rust-overlay; };
        new = pkgs.writeShellScriptBin "task_new" ''
          cat <<EOF
          cargo compete new "$1" \
              && git add "$1" \
              && git commit -m "feat: add $1" \
              && cd "$1"
          EOF
        '';
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            rustToolchain
            rust-analyzer
            statix
            cargo-compete
            new
          ];
        };

        formatter = pkgs.nixfmt-rfc-style;
      }
    );
}
