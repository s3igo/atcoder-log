# AtCoder Rust

Log of My AtCoder solutions written in Rust.

## Requirements

- direnv
- rustup
- GNU Make
- jq

## Usage

```sh
# Install cargo-compete
make install

# Init project
make init

# Update cargo-compete
make update

# Logout from AtCoder
make logout

# Check format of all contests with nightly rustfmt feature
make check

# Create new contest directory
cd contests/1.70.0
## only create new contest
cargo compete new <contest>
## or create new contest, commit and change directory
source new <contest>

# Execute Testcases
cargo compete t <problem>

# Submit code
cargo compete s <problem>
```
## Build

```sh
# check hashes and inputs
nix run 'nixpkgs#nix-init' -- \
    --url https://github.com/qryxip/cargo-compete \
    --config <(echo "nixpkgs = 'builtins.getFlake \"nixpkgs\"'")

nix run 'nixpkgs#nix-init' -- \
    --url https://github.com/hatoo/cargo-snippet \
    --config <(echo "nixpkgs = 'builtins.getFlake \"nixpkgs\"'")

```
