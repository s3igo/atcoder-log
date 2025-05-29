mod ac "tools/aclog"

_default:
    @just --choose

alias r := run
[no-exit-message]
run *args:
    @just ac::run {{ args }}

[no-exit-message]
test *args="--summary all":
    zig build test {{ args }}

clean:
    rm -rfv .zig-cache
