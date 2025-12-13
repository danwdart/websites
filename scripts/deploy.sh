#!/usr/bin/env bash
set -euo pipefail
trap pwd ERR

siteupdate() {
    git add .
    git commit -m 'Site update'
    git push
}

# TODO - ensure master and clean

cabal new-run build
# nix-build -A websites
# result/bin/build

cd .sites
for site in */
do
    cd "$site"
    siteupdate
    cd ..
done
cd ..

siteupdate
