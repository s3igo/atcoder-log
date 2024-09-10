import std/strformat

proc scanf(formatstr: cstring){.header: "<stdio.h>", varargs.}
proc scan(): int = scanf("%lld\n", addr result)

let
  a = scan()
  (b, c) = (scan(), scan())
  s = stdin.readLine

echo &"{a + b + c} {s}"
