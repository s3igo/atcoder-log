{ pkgs }:

with pkgs;
let
  test = writeShellApplication {
    name = "t";
    runtimeInputs = [
      time
      online-judge-tools
    ];
    text = ''
      cargo build --release && oj test --command './target/release/main'
    '';
  };
  submit = writeShellApplication {
    name = "s";
    runtimeInputs = [ online-judge-tools ];
    text = ''
      oj submit --no-open --yes -- "$URL" ./src/main.rs
    '';
  };
  testAndSubmit = writeShellApplication {
    name = "ts";
    runtimeInputs = [
      test
      submit
    ];
    text = ''
      t && s
    '';
  };
  nvim = writeShellApplication {
    name = "v";
    runtimeInputs = [ neovim ];
    text = ''
      nvim ./src/main.rs
    '';
  };
in
[
  test
  submit
  testAndSubmit
  nvim
]
