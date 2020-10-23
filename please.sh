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
    tezos_commit="46cae6f626e5b192fcbacb63da89f6a4fae004b7"
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
        git checkout "$tezos_commit"
        git log --oneline -n 5
        #echo "(data_only_dirs flextesa-lib) ;; Unvendored flextesa" > vendors/dune
    )
    say "Vendoring Vbmithr's base58 library"
    if [ -f "local-vendor/ocaml-base58/base58.opam" ] ; then
        say "Already cloned"
    else
        git clone --depth 10 https://github.com/vbmithr/ocaml-base58.git \
            local-vendor/ocaml-base58
    fi
    (
        cd local-vendor/ocaml-base58
        git pull
        git checkout src/base58.mli
        # We need to expose this for the Ledger-like hash:
        echo 'val raw_encode : ?alphabet:Alphabet.t -> string -> string' >> src/base58.mli
    )
    say "Vendoring Lwd++"
    if [ -f "local-vendor/lwd/lwd.opam" ] ; then
        say "Already cloned"
    else
        git clone --depth 10 https://github.com/let-def/lwd.git \
            local-vendor/lwd
    fi
    (
        cd local-vendor/lwd
        git pull
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
         digestif \
         js_of_ocaml-compiler js_of_ocaml-tyxml js_of_ocaml-lwt
}


eval $(opam env)

build_all () {
    mkdir -p _build/website/
    dune build --profile release src/client/main.bc.js
    cp --no-preserve mode _build/default/src/client/main.bc.js _build/website/main-client.js
    cp data/loading.gif _build/website/
    dune exec src/gen-web/main.exe index "TZComet" > _build/website/index.html
    echo "Done: file://$PWD/_build/website/index.html?lwd=true&dev=true"
    dune build src/deploy-examples/main.exe
}
build_ () {
    build_all
}

deploy_examples () {
    dune exec src/deploy-examples/main.exe "$@"
}

deploy_website () {
    build_all
    dst="$1"
    mkdir -p "$dst"
    cp _build/website/* "$dst/"
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

