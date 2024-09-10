proc scanf(formatstr: cstring){.header: "<stdio.h>", varargs.}
proc scan(): int = scanf("%lld\n", addr result)

func isOdd(n: int): bool = n mod 2 == 1
func cond(b: bool): string = (if b: "Odd" else: "Even")

let
  a = scan()
  b = scan()

echo (a * b).isOdd.cond
