#!/usr/bin/env bash

function build_cabal() {
    cabal sandbox init
    cabal install --only-dep
    cabal run clean
    cabal run build
}

function build_stack() {
    stack build
    stack exec -- site clean
    stack exec -- site build
}

case "${1}" in
    'stack' ) build_stack;;
    'cabal' ) build_cabal;;
    * ) exit 1;;
esac

GIT_USER=$( git config --get user.name )
GIT_EMAIL=$( git config --get user.email )
GIT_ORIGIN=$( git remote get-url origin )

if [[ -e _deploy ]]; then
    echo "_deploy exists, exiting..."
    exit 1
fi

mkdir ./_deploy/
(
    cd _deploy/
    git init
    git config user.name "${GIT_USER}"
    git config user.email "${GIT_EMAIL}"
    git remote add origin "${GIT_ORIGIN}"
    git fetch --all
    git checkout -b master origin/master
    git ls-files | xargs rm
    find ../_site/ -maxdepth 1 -mindepth 1 -print0 | xargs -0 cp -r -t .
    git add .
    git commit -m 'Scripted deployment...'
    git push
)
rm -rf _deploy

git fetch --all
