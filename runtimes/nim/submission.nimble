packageName   = "submission"
version       = "0.1.0"
author        = "s3igo"
description   = "Nim runtime for AtCoder submissions"
license       = "TBD"
bin           = @["main"]
backend       = "cpp"

# Dependencies

requires "nim == 1.6.14"
requires "neo == 0.3.4"
requires "bignum == 1.0.4"
requires "https://github.com/zer0-star/Nim-ACL#52761787964c590495eec34d366fd325afb4ae3e"
