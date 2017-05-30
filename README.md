# specter-edn

https://github.com/maitria/specter-edn/

Format-preserving Specter path for EDN and Cloure code.

There is one specter path: `SEXPRS`.  This navigates to a sequence of
s-expressions parsed from a string.

For the `transform` case, `specter-edn` preserves whitespace and comments using
a sort of diffing algorithm to match new S-expression parts to original parts
of the parse tree.

## Usage

```clojure
(use 'com.rpl.specter)
(use 'com.rpl.specter.macros)
(use 'specter-edn.core)

(select [SEXPRS FIRST LAST] "[42 26 77]")
;=> 77

(setval [SEXPRS FIRST FIRST] 99 "[42 ; Hi mom!\n6]")
;=> "[99 ; Hi mom!\n6]"
```

## TODO

* Use clj-format to format new code which isn't matched up with old code.

## License

Copyright © 2016, 2017 Jason M. Felice

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
