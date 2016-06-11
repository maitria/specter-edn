# specter-edn

Specter paths for working with formatted EDN and Cloure code.

There is one specter path: `SEXPR`.  This navigates to a sequence of
s-expressions parsed from a string.

For the `transform` case, `specter-edn` preserves whitespace and comments using
a sort of diffing algorithm to match new S-expression parts to original parts
of the parse tree.

## Usage

```clojure
(use 'com.rpl.specter)
(use 'com.rpl.specter.macros)
(use 'com.maitria.specter-edn)

(select [SEXPR FIRST LAST] "[42 26 77]")
;=> 77

(setval [SEXPR FIRST FIRST] 99 "[42 ; Hi mom!\n6]")
;=> "[99 ; Hi mom!\n6]"
```

## TODO

* Use clj-format to format new code which isn't matched up with old code.

## License

Copyright © 2016 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
