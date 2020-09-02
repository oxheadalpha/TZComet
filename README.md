# Comevitz: Contract Metadata Viewer on Tezos


![comevitz-20200902](https://user-images.githubusercontent.com/617111/91987948-b0c78180-ecfc-11ea-8be4-00b54ced0fff.gif)

… currently running at <https://wr.mondet.org/comevitz/> …

## Build

    ./please.sh ensure vendors  # Clones a branch of Tezos
    ./please.sh ensure setup    # Creates a local opam switch
    ./please.sh build all
   
⬑ if all goes well, last command should print out a link to open the app, like
`file://$PWD/_build/default/src/client/index.html`.

## See Also

The development of the contract metadata standard (TZIP-16) at
[`tzip/tzip!76`](https://gitlab.com/tzip/tzip/-/merge_requests/76) and
[`smondet/tezos!7`](https://gitlab.com/smondet/tezos/-/merge_requests/7).


