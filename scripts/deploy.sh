#!/usr/bin/env bash
siteupdate() {
    git add .
    git commit -m 'Site update'
    git push
}

# TODO - ensure master and clean

# cabal new-run build
nix-build
result/bin/build

cd .sites
for site in */
do
    cd $site
    siteupdate
    cd ..
done
cd ..

siteupdate
