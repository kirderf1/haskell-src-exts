This is a fork of haskell-src-exts that has been extended with an experimental language extension. If you are looking for the repository of the original haskell-src-exts package, you can find it [here][suite-hse].

[suite-hse]: https://github.com/haskell-suite/haskell-src-exts

Haskell Source Extensions
=========================

haskell-src-exts is a package for handling and manipulating Haskell source
code. It is a descendant of the haskell-src package that is part of the standard
libraries, but extends this to support a number of syntactic
extensions, e.g. MPTCs, fundeps, GADTs, TH etc. The aim is to support all
extensions recognized by the community, as determined by what is implemented
in compilers and tools.

Apart from the more standard extensions supported by e.g. GHC,
haskell-src-exts provides support for HaRP (Haskell Regular Patterns)
and HSX (Haskell Source with XML) syntax.

This specific fork of haskell-src-exts is a part of [composable-types][cty], and has been extended to represent and parse experimental syntax relating to [composable-types][cty].

[cty]: https://github.com/kirderf1/composable-types

Package structure
-----------------

The modules that comprise haskell-src-exts all reside in the hierarchic
namespace Language.Haskell.Exts. Notable exposed modules include:

* `Language.Haskell.Exts` - Imports and re-exports all the below,
  and also defines some functions that combine functionality from several
  modules.
* `Language.Haskell.Exts.Syntax` - The abstract syntax tree
  that the other modules work on.
* `Language.Haskell.Exts.Build` - Combinators for building
  abstract syntax.
* `Language.Haskell.Exts.Parser` - Functions for parsing Haskell
  source code into an abstract syntax representation.


License
-------

The haskell-src-exts Package is distributed under a derived BSD-style license. It
derives from several sources, all of which are distributable under
BSD-style or compatible licenses. See the file LICENSE for the complete
license text.
