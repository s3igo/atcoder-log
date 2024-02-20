{
  description = "AtCoder development environment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixvim = {
      url = "github:nix-community/nixvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    dotfiles.url = "github:s3igo/dotfiles";
  };

  outputs =
    {
      self,
      nixpkgs,
      fenix,
      nixvim,
      dotfiles,
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
            default.rustfmt # rustfmt nightly
          ];
        neovim = nixvim.legacyPackages.${system}.makeNixvimWithModule {
          module = {
            imports = [ dotfiles.nixosModules.neovim ];

            plugins.lsp.servers.rust-analyzer = {
              enable = true;
              installCargo = false;
              installRustc = false;
              cmd = [ "rust-analyzer" ];
              filetypes = [ "rust" ];
              rootDir = ''
                function()
                  return vim.fs.dirname(vim.fs.find({ 'Cargo.toml', '.git' }, { upward = true })[1])
                end
              '';
              settings = {
                check.command = "clippy";
                files.excludeDirs = [ ".direnv" ];
              };
            };
          };
        };
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
        packages = {
          inherit neovim;
          default = neovim;
        };

        devShell = pkgs.mkShell {
          buildInputs =
            let
              deps = [
                neovim
                toolchain
                cargo-compete
                cargo-snippet
              ];
            in
            with pkgs;
            [ statix ] ++ deps ++ tasks;
        };

        formatter = pkgs.nixfmt-rfc-style;
      }
    );
}
