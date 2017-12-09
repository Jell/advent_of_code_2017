# Advent of Code 2017

Solutions in Clojure, ~~PureScript~~, Idris.

You need Idris compiled with ffi support (for type providers):

```
cabal install idris-1.1.1 -f ffi
```

You also need to install the Lightyear package:

https://github.com/ziman/lightyear

## Log

### Gave up on PureScript

Gave up on PureScript after day 3 because of a bug that bothered me
for too long. It boils down to this raising `RangeError: Maximum call
stack size exceeded`:

```purescript
import Prelude
import Data.List.Lazy
index (iterate ((+) 1) 1) 10000
```
