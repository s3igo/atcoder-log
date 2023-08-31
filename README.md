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
make new ARG=<contest>

# Execute Testcases
cargo compete t <problem>

# Submit code
cargo compete s <problem>
```
