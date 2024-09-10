import std/[sequtils, sugar]

proc scanf(formatstr: cstring){.header: "<stdio.h>", varargs.}
proc scan(): int = scanf("%lld\n", addr result)

let
  a = scan()
  b = scan()
  c = scan()
  x = scan()

  ans = collect:
    for i in 0..a:
      for j in 0..b:
        for k in 0..c:
          i * 500 + j * 100 + k * 50 == x

echo ans.countIt(it)
