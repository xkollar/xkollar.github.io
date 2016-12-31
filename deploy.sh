#!/usr/bin/env bash

cabal sandbox init
cabal install --only-dep
cabal run build

GIT_USER=$( git config --get user.name )
GIT_EMAIL=$( git config --get user.email )

if [[ -e _deploy ]]; then
    echo "_deploy exists, exiting..."
fi

mkdir ./_deploy/
(
    cd _deploy/
    git init
    git config user.name "$GIT_USER"
    git config user.email "$GIT_EMAIL"
    git remote add origin git@github.com:xkollar/xkollar.github.io.git
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
