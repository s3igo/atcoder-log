mod ac "tools/aclog"

_default:
    @just --choose

alias r := run
[no-exit-message]
run *args:
    @just ac::run {{ args }}

test *args:
    zig build test {{ args }}
