alias r := run
[no-exit-message]
run *args:
    cabal v2-run --offline {{ args }}
