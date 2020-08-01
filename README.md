# `th-compat`
[![Hackage](https://img.shields.io/hackage/v/th-compat.svg)][Hackage: th-compat]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/th-compat.svg)](http://packdeps.haskellers.com/reverse/th-compat)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]
[![Build](https://img.shields.io/travis/haskell-compat/th-compat.svg)](https://travis-ci.org/haskell-compat/th-compat)

[Hackage: th-compat]:
  http://hackage.haskell.org/package/th-compat
  "th-compat package on Hackage"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"

This package defines a `Language.Haskell.TH.Syntax.Compat`
module, which backports the `Quote` and `Code` types to
work across a wide range of `template-haskell` versions.
On recent versions of `template-haskell` (2.17.0.0 or
later), this module simply reexports `Quote` and `Code`
from `Language.Haskell.TH.Syntax`. Refer to the Haddocks
for `Language.Haskell.TH.Syntax.Compat` for examples of
how to use this module.
