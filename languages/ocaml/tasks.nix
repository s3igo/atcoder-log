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
      ocamlfind ocamlopt -O2 -o a.out main.ml -linkpkg -thread \
        -package str,num,threads,containers,core,iter,batteries \
        && oj test
    '';
  };
  submit = writeShellApplication {
    name = "s";
    runtimeInputs = [ online-judge-tools ];
    text = ''
      oj submit --no-open --yes -- "$URL" ./main.ml
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
    text = ''
      nvim ./main.ml
    '';
  };
in
[
  test
  submit
  testAndSubmit
  nvim
]
