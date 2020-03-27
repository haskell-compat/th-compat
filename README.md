# `th-quote`
[![Hackage](https://img.shields.io/hackage/v/th-quote.svg)][Hackage: th-quote]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/th-quote.svg)](http://packdeps.haskellers.com/reverse/th-quote)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]
[![Build](https://img.shields.io/travis/haskell-compat/th-quote.svg)](https://travis-ci.org/haskell-compat/th-quote)

[Hackage: th-quote]:
  http://hackage.haskell.org/package/th-quote
  "th-quote package on Hackage"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"

This package defines a `Language.Haskell.TH.Syntax.Quote` module providing a
`Quote` class that works across all versions of `template-haskell`. On recent
versions of `template-haskell` (2.17.0.0 or later), this module simply
reexports `Quote` from `Language.Haskell.TH.Syntax`. Refer to the Haddocks
for `Language.Haskell.TH.Syntax.Quote` for an example of how to use this
module.
