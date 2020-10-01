# TZComet: Tezos Contract Metadata

<div>
<a href="https://user-images.githubusercontent.com/617111/94707034-f59d0300-0310-11eb-805b-38cdc8038e30.gif"><img
  width="33%"
  src="https://user-images.githubusercontent.com/617111/94707034-f59d0300-0310-11eb-805b-38cdc8038e30.gif"
></a>
<a href="https://user-images.githubusercontent.com/617111/94707033-f5046c80-0310-11eb-9155-0d81ea5ed141.gif"><img width="33%"
 src="https://user-images.githubusercontent.com/617111/94707033-f5046c80-0310-11eb-9155-0d81ea5ed141.gif" ></a>
<a href="https://user-images.githubusercontent.com/617111/94707030-f46bd600-0310-11eb-8bbb-a2552c6c7f7b.gif"><img width="33%"
 src="https://user-images.githubusercontent.com/617111/94707030-f46bd600-0310-11eb-8bbb-a2552c6c7f7b.gif" ></a>
</div>

… currently running at <https://tqtezos.github.io/TZComet/> …

## Build

    ./please.sh ensure vendors  # Clones a branch of Tezos and vbmithr/ocaml-base58
    ./please.sh ensure setup    # Creates a local opam switch
    ./please.sh build all
   
⬑ if all goes well, last command should print out a link to open the app, like
`file://$PWD/_build/default/src/client/index.html`.

## See Also

- Agora [post](https://forum.tezosagora.org/t/contract-metadata-on-tezos/2258)
  introducing the specification.
- The
  [TZIP-16](https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-16/tzip-16.md)
  specification.
- A work-in-progress merge-request for `tezos/tezos` on which this development
  is based:
  [`smondet/tezos!7`](https://gitlab.com/smondet/tezos/-/merge_requests/7).


