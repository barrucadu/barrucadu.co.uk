---
title: "cabal-info: get information from cabal files."
description: Have you ever needed to get information from a cabal file
  in a shell script? Now you can! `cabal-info` exposes a simple
  command-line interface to the cabal file format.
---

([GitHub link for the impatient.][git])

The other day I was writing a shell-script, and needed some
information from a cabal file. This was all fine and dandy when I just
wanted the name, version, or synopsis; but as soon as I wanted
multi-line fields (like the description or build-depends), things
became a bit more hairy. My incantations of `cat` and `grep` and `sed`
were getting cumbersome, so I looked for a program to do this, but
couldn't find one! This told me two things:

1. My original goal was probably too complicated for a reasonable
   shell script.
2. I would need to write a program to do this myself if I wanted it.

1 encouraged me to stop, think, and start again in a *real*
programming language. But I had started, and so by God I would finish!
This led to 2, which led to this program.

Let's dive straight in to some examples.

Basics
------

Here's a simple cabal file:

```
name:           HUnit
version:        1.1.1
synopsis:       A unit testing framework for Haskell
homepage:       http://hunit.sourceforge.net/
category:       Testing
author:         Dean Herington
license:        BSD3
license-file:   LICENSE
cabal-version:  >= 1.10
build-type:     Simple

library
  build-depends:      base >= 2 && < 4
  exposed-modules:    Test.HUnit.Base, Test.HUnit.Lang,
                      Test.HUnit.Terminal, Test.HUnit.Text, Test.HUnit
  default-extensions: CPP
```

Here are some simple queries:

```
$ cabal-info name
HUnit

$ cabal-info build-depends
base >=2 && <4
```

Multi-value fields are displayed with each value on a new line:

```
$ cabal-info exposed-modules
Test.HUnit.Base
Test.HUnit.Lang
Test.HUnit.Terminal
Test.HUnit.Text
Test.HUnit
```

Sections
--------

Now let's look at a more complex example:

```
name:            TestPackage
version:         0.0
synopsis:        Package with library and two programs
license:         BSD3
author:          Angela Author
build-type:      Simple
cabal-version:   >= 1.2

library
  build-depends:   HUnit
  exposed-modules: A, B, C

executable program1
  main-is:         Main.hs
  hs-source-dirs:  prog1
  other-modules:   A, B

executable program2
  main-is:         Main.hs
  hs-source-dirs:  prog2
  other-modules:   A, C, Utils
```

Clearly we can't use just "main-is" here to refer to both programs, we
need to disambiguate. We can use the executable (or source repository,
or test suite, or benchmark) name as a prefix:

```
$ cabal-info program1:hs-source-dirs
prog1

$ cabal-info program2:hs-source-dirs
prog2
```

There are some special cases for common usages:

- "executable"/"test-suite"/"benchmark" refers to the name of the
  first executable/test suite/benchmark.
- "main-is" refers to the "main-is" field of the first executable.
- "upstream" refers to the "location" of the head source repository.

Behold:

```
$ cabal-info executable
program1
```

Flags
-----

Flags? No problem:

```
Name: Test1
Version: 0.0.1
Cabal-Version: >= 1.2
License: BSD3
Author:  Jane Doe
Synopsis: Test package to test configurations
Category: Example

Flag Debug
  Description: Enable debug support
  Default:     False

Flag WebFrontend
  Description: Include API for web frontend.
  -- Cabal checks if the configuration is possible, first
  -- with this flag set to True and if not it tries with False

Library
  Build-Depends:   base
  Exposed-Modules: Testing.Test1
  Extensions:      CPP

  if flag(debug)
    GHC-Options: -DDEBUG
    if !os(windows)
      CC-Options: "-DDEBUG"
    else
      CC-Options: "-DNDEBUG"

  if flag(webfrontend)
    Build-Depends: cgi > 0.42
    Other-Modules: Testing.WebStuff

Executable test1
  Main-is: T1.hs
  Other-Modules: Testing.Test1
  Build-Depends: base

  if flag(debug)
    CC-Options: "-DDEBUG"
    GHC-Options: -DDEBUG
```

The "debug" flag defaults to false, and "webfrontend" to true:

```
$ cabal-info test1:cc-options
 
$ cabal-info -fdebug test1:cc-options
-DDEBUG

$ cabal-info build-depends
base -any && -any
cgi >0.42

$ cabal-info -f-webfrontend build-depends
base -any && -any
```

Note: there isn't any way to specify the operating system yet, the OS
of the build system is used instead. I'll probably fix this tomorrow,
if not tonight.

Pretty-printing
---------------

Finally, we can pretty-print an entire cabal file, with flags
applied. This is useful for seeing the effects of different
combinations of flags:

```
$ cabal-info
name:          Test1
version:       0.0.1
build-depends: base -any && -any
             , cgi >0.42
license:       BSD3
author:        Jane Doe
synopsis:      Test package to test configurations
category:      Example

library
  exposed:         True
  exposed-modules: Testing.Test1
  build-depends:   base -any
                 , cgi >0.42
  other-modules:   Testing.WebStuff
  hs-source-dirs:  .
  extensions:      CPP
  buildable:       True

executable test1
  main-is:        T1.hs
  build-depends:  base -any
  other-modules:  Testing.Test1
  hs-source-dirs: .
  buildable:      True


$ cabal-info -f"debug -webfrontend"
name:          Test1
version:       0.0.1
build-depends: base -any && -any
             , cgi >0.42
license:       BSD3
author:        Jane Doe
synopsis:      Test package to test configurations
category:      Example

library
  exposed:         True
  exposed-modules: Testing.Test1
  build-depends:   base -any
                 , cgi >0.42
  other-modules:   Testing.WebStuff
  hs-source-dirs:  .
  extensions:      CPP
  buildable:       True
  ghc-options:     -DDEBUG
  cc-options:      -DDEBUG

executable test1
  main-is:        T1.hs
  build-depends:  base -any
  other-modules:  Testing.Test1
  hs-source-dirs: .
  buildable:      True
  ghc-options:    -DDEBUG
  cc-options:     -DDEBUG
```

This is all [on GitHub][git]. I'll probably tidy it up and add it to
{h,st}ackage, after verifying that there *really is* no established
tool to do this.

That's all, folks!

[git]: https://github.com/barrucadu/cabal-info
