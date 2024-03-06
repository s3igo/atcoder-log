{ nixpkgs, system }:

let
  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
  };
in

with pkgs;
let
  build = writeShellApplication {
    name = "task_build";
    runtimeInputs = [ _1password ];
    text = ''
      PROJ_ROOT=$(git rev-parse --show-toplevel)
      docker build \
        --build-arg ATTIC_TOKEN="$(sudo cat /run/agenix/attic-token)" \
        --build-arg COPILOT_TOKEN="$(cat "$XDG_CONFIG_HOME/github-copilot/hosts.json")" \
        --build-arg ATCODER_USERNAME="$(op read op://Personal/AtCoder/username)" \
        --build-arg ATCODER_PASSWORD="$(op read op://Personal/AtCoder/password)" \
        --tag s3igo/atcoder-rust \
        "$PROJ_ROOT/containers/rust"
    '';
  };
  run = writeShellApplication {
    name = "task_run";
    text = ''
      PROJ_ROOT=$(git rev-parse --show-toplevel)
      docker run --rm -it \
        --mount type=bind,source="$PROJ_ROOT/snippets/rustfmt.toml",target=/workspace/rustfmt.toml \
        s3igo/atcoder-rust "$@"
    '';
  };
  update = writeShellApplication {
    name = "task_update";
    text = ''
      PROJ_ROOT=$(git rev-parse --show-toplevel)
      docker run --rm -it \
        --mount type=bind,source="$PROJ_ROOT/flake.lock",target=/workspace/flake.lock \
        s3igo/atcoder-rust \
        nix flake update
    '';
  };
  open = writeShellApplication {
    name = "task_open";
    text = ''
      # $1: task url (optional)
      # $2: filename (optional)
      # at least one of them is required

      [[ $1 == https://atcoder.jp/* ]]
      URL_IS_SPECIFIED=$?

      [ $URL_IS_SPECIFIED = 0 ] \
        && FILENAME=''${2:-$(basename "$1").rs} \
        || FILENAME="$1"

      [ -f "$FILENAME" ] || cat > "$FILENAME" <<EOF
      use proconio::input;

      fn main() {
          input!();
      }
      EOF

      # NOTE: redundant command due to the complexity of [COMMAND] and [ARG...]
      # received by `docker run`
      PROJ_ROOT=$(git rev-parse --show-toplevel)
      if [ $URL_IS_SPECIFIED = 0 ]; then
        docker run --rm -it \
          --mount type=bind,source="$PROJ_ROOT/snippets/rustfmt.toml",target=/workspace/rustfmt.toml \
          --mount type=bind,source="$(pwd)/$FILENAME",target=/workspace/src/main.rs \
          --env URL="$1" \
          s3igo/atcoder-rust \
          nix develop --command fish --init-command "oj download $1 && nvim ./src/main.rs"
      else
        docker run --rm -it \
          --mount type=bind,source="$PROJ_ROOT/snippets/rustfmt.toml",target=/workspace/rustfmt.toml \
          --mount type=bind,source="$(pwd)/$FILENAME",target=/workspace/src/main.rs \
          s3igo/atcoder-rust \
          nix develop --command fish --init-command 'nvim ./src/main.rs'
      fi
    '';
  };
in
[
  build
  run
  update
  open
]
