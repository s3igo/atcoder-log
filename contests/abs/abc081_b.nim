import std/sequtils

proc scanf(formatstr: cstring){.header: "<stdio.h>", varargs.}
proc scan(): int = scanf("%lld\n", addr result)

func isEven(n: int): bool = n mod 2 == 0

let n = scan()
var a = newSeqWith(n, scan())

var cnt = 0
while a.all(isEven):
  cnt += 1
  a = a.mapIt(it div 2)

echo cnt
