alias b := build
[no-exit-message]
build *args:
    dotnet build {{ args }}

alias r := run
[no-exit-message]
run *args:
    dotnet run {{ args }}
