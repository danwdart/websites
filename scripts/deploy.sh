#!/usr/bin/env bash
siteupdate() {
    git add .
    git commit -m 'Site update'
    git push
}

# TODO - ensure master and clean

cabal new-run build
# nix-build -A websites
# result/bin/build

cd .sites || exit
for site in */
do
    cd "$site" || exit
    siteupdate
    cd ..
done
cd ..

siteupdate
