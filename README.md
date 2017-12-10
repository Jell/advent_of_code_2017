# Advent of Code 2017

Solutions in Clojure, ~~PureScript~~, Idris.

You need Idris compiled with ffi support (for type providers):

```
cabal install idris-1.1.1 -f ffi
```

You also need to install the Lightyear package:

https://github.com/ziman/lightyear

## Log

### Day 1

Trivial and a pleasure in Clojure.
Boiler-platy in PureScript, without gaining anything.
Fun in Idris with totality check!

### Day 2

Trivial and a pleasure in Clojure.
Trivial and a pleasure in PureScript.
Trivial and a pleasure in Idris, with totality check!!

### Day 3

Nice and a pleasure in Clojure.
Nice in Idris, most of the code is totality checked.
Nightmare in PureScript because bug.

Gave up on PureScript after that because of a bug that bothered me for
too long. It boils down to this raising `RangeError: Maximum call
stack size exceeded`:

```purescript
import Prelude
import Data.List.Lazy
index (iterate ((+) 1) 1) 10000
```

### Day 4

Easy in Clojure.
Trivial in Idris! Turned out as smooth as Clojure!

### Day 5

Painful and slow in Idris.
Easy and fast in Clojure.

### Day 6

Easy and fast in Clojure (with tests).
Was ok in Idris, not the most fun.

### Day 7

Was painful but fun in Idris, just painful in Clojure
Built a tower that's proven to be balanced using Idris!
Messy solution in Clojure, types would have helped.

### Day 8

Trivial in Clojure, `reductions` turned out to be super useful.
Very boiler-platy in Idris, kinda fun with State monad.

### Day 9

Easy and nice in Clojure using regexp.
Was hard but kinda nice with parser combinator in Idris.
