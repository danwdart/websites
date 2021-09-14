#!/usr/bin/env bash
siteupdate() {
    git add .
    git commit -m 'Site update'
    git push
}

# TODO - ensure master and clean

# cabal new-run build-websites
nix-build
result/bin/build-websites

cd .sites
for site in */
do
    cd $site
    siteupdate
    cd ..
done
cd ..

siteupdate
