proc scanf(formatstr: cstring){.header: "<stdio.h>", varargs.}
proc scan(): int = scanf("%lld\n", addr result)
