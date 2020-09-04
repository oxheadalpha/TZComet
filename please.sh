#!/bin/sh

set -e

usage () {
    cat >&2 <<EOF
./please.sh <cmd>

EOF
}

say () {
    printf "please.sh: %s\n" "$*" >&2
}

ensure_vendors () {
    say "Ensuring vendors"
    # We use the following pointer to the main repo's master branch:
    tezos_branch=smondet-contract-metadata
    tezos_remote=https://gitlab.com/smondet/tezos.git
    say "Vendoring tezos @ %10s" "$tezos_branch"
    if [ -f "local-vendor/tezos/README.md" ] ; then
        say "Tezos already cloned"
    else
        mkdir -p local-vendor/
        git clone --depth 10 "$tezos_remote" -b "$tezos_branch" \
            local-vendor/tezos
    fi
    (
        cd local-vendor/tezos/
        git checkout "$tezos_branch"
        git pull
        git log --oneline -n 5
        #echo "(data_only_dirs flextesa-lib) ;; Unvendored flextesa" > vendors/dune
    )
}

ensure_setup () {
    if ! [ -d _opam/ ] ; then
        opam switch create . ocaml-base-compiler.4.09.1
    fi
    eval $(opam env)
    opam install --deps-only \
         local-vendor/tezos/src/lib_contract_metadata/core/tezos-contract-metadata.opam
    opam install -y base fmt uri cmdliner ezjsonm \
         ocamlformat uri merlin ppx_deriving angstrom \
         zarith_stubs_js \
         js_of_ocaml-compiler js_of_ocaml-tyxml js_of_ocaml-lwt
}


eval $(opam env)

build_all () {
    dune build src/client/index.html
    echo "Done: file://$PWD/_build/default/src/client/index.html"
}
build_ () {
    build_all
}

deploy_website () {
    build_all
    dst="$1"
    mkdir -p "$dst"
    cp _build/default/src/client/index.html "$dst/"
    cp _build/default/src/client/main.bc.js "$dst/"
    cp _build/default/src/client/loading.gif "$dst/"
    chmod a+w "$dst/"*
    git describe --always HEAD > "$dst/VERSION"
    echo "Done → $dst"
}

{
    case "$1" in
        "" | "--help" | "help" | "usage" )
            usage ;;
        "ensure" | "build" | "deploy" )
            cmd="$1_$2"
            shift 2
            "$cmd" "$@" ;;
        * )
            "$@" ;;
    esac
}

