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
    tezos_commit="8d3aa42b9628e4d3440556c85b4fd0f75298bcd2"
    say "Vendoring tezos @ %10s" "$tezos_branch"
    if [ -f "local-vendor/tezos/README.md" ] ; then
        say "Tezos already cloned"
    else
        mkdir -p local-vendor/
        git clone "$tezos_remote" -b "$tezos_branch" \
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
    lwd_commit="3bfeca4d37560f8778610c51d60d2d1aedfa519d"
    if [ -f "local-vendor/lwd/lwd.opam" ] ; then
        say "Already cloned"
    else
        git clone --depth 30 https://github.com/let-def/lwd.git \
            local-vendor/lwd
    fi
    (
        cd local-vendor/lwd
        git pull
        git pull
        git checkout "$lwd_commit"
        git log --oneline -n 5
    )
}

ocamlformat_version=0.19.0
ensure_setup () {
    if [ "$global_switch" = "true" ] ; then
        say "Assuming Global Opam Switch is set"
    else
        if ! [ -d _opam/ ] ; then
            opam switch create . ocaml-base-compiler.4.09.1
        fi
    fi
    eval $(opam env)
    opam pin add -n digestif 0.9.0
    opam pin add -n ocamlformat "$ocamlformat_version"
    opam pin add -n tyxml 4.5.0
    opam pin add -n zarith 1.11 # zarith_stubs_js fails with 1.12
    opam pin add -n js_of_ocaml-lwt 3.11.0
    # see https://github.com/janestreet/zarith_stubs_js/pull/8
    opam install --deps-only \
         local-vendor/tezos/src/lib_contract_metadata/tezos-contract-metadata.opam
    opam install -y base fmt uri cmdliner ezjsonm \
         ocamlformat uri merlin ppx_deriving angstrom \
         ppx_inline_test lwt-canceler.0.3 zarith_stubs_js \
         digestif tyxml \
         js_of_ocaml-compiler js_of_ocaml-lwt
}


eval $(opam env)

root_path=${root:-.}
dune_profile=${profile:-release}

build_all () {
    eval $(opam env)
    dune build @check
    mkdir -p _build/website/
    dune build --profile "$dune_profile" $root_path/src/client/main.bc.js
    cp _build/default/$root_path/src/client/main.bc.js _build/website/main-client.js
    cp $root_path/data/loading.gif _build/website/
    dune build $root_path/src/client/index.html
    cp _build/default/$root_path/src/client/index.html _build/website/
    chmod 600 _build/website/*
    echo "Done: file://$PWD/_build/website/index.html"
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

deploy_togithub () {
    localpath="staging"
    if [ "$prod" = "true" ] ; then
        localpath="."
    else
        mkdir -p "$localpath"
    fi
    dst=$(mktemp -d -p /tmp comevitz-XXX)
    if [ "$prod" = "true" ] ; then
        git checkout origin/master
    fi
    ./please.sh deploy website "$dst"
    # First time: git checkout --orphan gh-pages
    git checkout gh-pages
    mv "$dst/"* "$localpath"
    (
        cd "$localpath"
        git add index.html main-client.js loading.gif VERSION
    )
    msg="(Staging)"
    if [ "$prod" = "true" ] ; then
        msg="(Production)"
    fi
    git commit -m "Deploy $(cat "$localpath"/VERSION) $msg"
    say "Current branch in gh-pages, it is not pushed."
}


ensure_ocamlformats () {
    command="${1:-cp}"
    tmp=$(mktemp /tmp/XXXXX.ocamlformat)
    cat > "$tmp" <<EOF
version=$ocamlformat_version
profile=compact
break-collection-expressions=fit-or-vertical
exp-grouping=preserve
parse-docstrings
EOF
    for dotof in $(git ls-files | grep .ocamlformat) ; do
        $command "$tmp" "$dotof" || {
            echo "File '$dotof' should be:" >&2
            cat "$tmp" | sed 's/^/  ||/' >&2
            echo "You may have to run './please.sh ensure ocamlformats'" >&2
            return 4
        }
    done
}


ensure_linting () {
    echo "OCamlFormat version: $(ocamlformat --version)"
    ensure_ocamlformats "diff --brief"
    dune build @src/fmt --auto-promote
}

{
    case "$1" in
        "" | "--help" | "help" | "usage" )
            usage ;;
        "ensure" | "build" | "deploy"  )
            cmd="$1_$2"
            shift 2
            "$cmd" "$@" ;;
        * )
            "$@" ;;
    esac
}
