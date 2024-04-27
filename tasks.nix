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

      case $1 in
        rust)
          shift
          docker build \
            --build-arg ATTIC_TOKEN="$(sudo cat /run/agenix/attic-token)" \
            --build-arg COPILOT_TOKEN="$(cat "$XDG_CONFIG_HOME/github-copilot/hosts.json")" \
            --build-arg ATCODER_USERNAME="$(op read op://Personal/AtCoder/username)" \
            --build-arg ATCODER_PASSWORD="$(op read op://Personal/AtCoder/password)" \
            --tag atcoder/rust \
            "$@" \
            "$PROJ_ROOT/containers/rust"
          ;;
        ocaml)
          shift
          docker build \
            --build-arg ATTIC_TOKEN="$(sudo cat /run/agenix/attic-token)" \
            --build-arg COPILOT_TOKEN="$(cat "$XDG_CONFIG_HOME/github-copilot/hosts.json")" \
            --build-arg ATCODER_USERNAME="$(op read op://Personal/AtCoder/username)" \
            --build-arg ATCODER_PASSWORD="$(op read op://Personal/AtCoder/password)" \
            --tag atcoder/ocaml \
            "$@" \
            "$PROJ_ROOT/containers/ocaml"
          ;;
        *)
          echo "Unsupported language: $1"
          ;;
      esac
    '';
  };
  run = writeShellApplication {
    name = "task_run";
    text = ''
      case $1 in
        rust)
          shift
          docker run --rm -it -p 2222:2222 atcoder/rust "$@"
          ;;
        ocaml)
          shift
          docker run --rm -it -p 2222:2222 atcoder/ocaml "$@"
          ;;
        *)
          echo "Unsupported language: $1"
          ;;
      esac
    '';
  };
  update = writeShellApplication {
    name = "task_update";
    text = ''
      PROJ_ROOT=$(git rev-parse --show-toplevel)

      case $1 in
        rust)
          docker run --rm -it \
            --mount type=bind,source="$PROJ_ROOT/flake.lock",target=/workspace/flake.lock \
            atcoder/rust \
            nix flake update
          ;;
        ocaml)
          docker run --rm -it \
            --mount type=bind,source="$PROJ_ROOT/flake.lock",target=/workspace/flake.lock \
            atcoder/ocaml \
            nix flake update
          ;;
        *)
          echo "Unsupported language: $1"
          ;;
      esac
    '';
  };
  open = writeShellApplication {
    name = "task_open";
    text = ''
      # $1: language
      # $2: task url (optional)
      # $3: filename (optional)
      # $2, $3 are optional but one of them is required

      function rust() {
        if [[ $1 == https://atcoder.jp/* ]]; then
          FILENAME=''${2:-$(basename "$1").rs}
          URL="$1"
        else
          FILENAME="$1"
          URL="https://atcoder.jp/contests/$(basename "$PWD")/tasks/''${1%.*}"
        fi

        [ -f "$FILENAME" ] || cat > "$FILENAME" <<EOF
use proconio::input;

fn main() {
    input!();
}
EOF

        docker run --rm -it \
          --mount type=bind,source="$(pwd)/$FILENAME",target=/workspace/src/main.rs \
          --env URL="$URL" \
          atcoder/rust \
          nix develop --command fish --init-command "oj download $URL; nvim ./src/main.rs"
      }

      function ocaml() {
        if [[ $1 == https://atcoder.jp/* ]]; then
          FILENAME=''${3:-$(basename "$1").ml}
          URL="$1"
        else
          FILENAME="$1"
          URL="https://atcoder.jp/contests/$(basename "$PWD")/tasks/''${1%.*}"
        fi

        [ -f "$FILENAME" ] || cat > "$FILENAME" <<EOF
let () =
EOF

        docker run --rm -it \
          --mount type=bind,source="$(pwd)/$FILENAME",target=/workspace/src/main.ml \
          --env URL="$URL" \
          atcoder/ocaml \
          nix develop --command fish --init-command "oj download $URL; nvim ./main.ml"
      }

      case $1 in
        rust)
          shift
          rust "$@"
          ;;
        ocaml)
          shift
          ocaml "$@"
          ;;
        *)
          echo "Unsupported language: $1"
          ;;
      esac

    '';
  };
in
[
  build
  run
  update
  open
]
