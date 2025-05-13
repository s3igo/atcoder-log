alias b := build
[no-exit-message]
build *args:
    zig build {{ args }}

alias c := check
[no-exit-message]
check *args:
    zig build check {{ args }}

alias t := test
[no-exit-message]
test *args:
    zig build test {{ args }}

alias r := run
[no-exit-message]
run *args:
    zig build run {{ args }}
