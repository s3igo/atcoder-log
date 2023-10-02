# AtCoder Rust

Log of My AtCoder solutions written in Rust.

## Requirements

- direnv
- rustup

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
