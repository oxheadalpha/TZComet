# TZComet: Tezos Contract Metadata

<div><a href="https://user-images.githubusercontent.com/617111/97904884-235dd900-1d0f-11eb-9aae-28e60eb0af15.gif"><img
 width="80%"
  src="https://user-images.githubusercontent.com/617111/97904884-235dd900-1d0f-11eb-9aae-28e60eb0af15.gif"
></a></div>

… currently running at <https://tzcomet.io> …

## Build

    ./please.sh ensure vendors  # Clones a branch of some deps (incl. Tezos)
    ./please.sh ensure setup    # Creates a local opam switch
    ./please.sh build all

⬑ if all goes well, last command should print out a link to open the app, like
`file://$PWD/_build/default/website/index.html`. l


## Note

The module Tzcomet_jsonm was copied from:

- repository: <https://github.com/hhugo/tree/jsoo-friendly>
- branch: jsoo-friendly
- commit: a092b96d20302ffa50c1f10c2ac6bf81c7cff9cf

This fork of Jsonm fixes the stack overflow error that can occur when parsing large objects in JSOO.


## See Also

- Agora [post](https://forum.tezosagora.org/t/contract-metadata-on-tezos/2258)
  introducing the specification.
- The
  [TZIP-16](https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-16/tzip-16.md)
  specification.
