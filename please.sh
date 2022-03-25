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


ocamlformat_version=0.19.0
ensure_setup () {
    if [ "$global_switch" = "true" ] || [ -d _opam ] ; then
        say 'Opam switch already there'
    else
        opam switch create tzcomet-413 \
             --formula='"ocaml-base-compiler" {>= "4.13" & < "4.14"}'
        opam switch link tzcomet-413 .
    fi
    eval $(opam env)
    opam pin add -n digestif 0.9.0
    opam pin add -n ocamlformat "$ocamlformat_version"
    # zarith_stubs_js fails with later version of those 2:
    opam pin add -n zarith 1.11
    opam pin add -n zarith_stubs_js v0.14.1
    # The older compiler does not work with recent dune:
    opam pin add -n js_of_ocaml-compiler 4.0.0
    tezais="lwd-bootstrap lwd-bootstrap-generator tezai-base58-digest tezai-michelson tezai-contract-metadata tezai-contract-metadata-manipulation"
    for tezai in $tezais ; do
        opam pin add -n $tezai https://gitlab.com/oxheadalpha/$tezai.git
    done
    # see https://github.com/janestreet/zarith_stubs_js/pull/8
    opam install -y base fmt uri cmdliner ezjsonm \
         ocamlformat uri merlin ppx_deriving angstrom \
         ppx_inline_test lwt-canceler.0.3 zarith_stubs_js \
         digestif tyxml tyxml-lwd \
         js_of_ocaml-compiler js_of_ocaml-lwt
    opam install -y $tezais
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
