#!/bin/bash
siteupdate() {
    git add .
    git commit -m 'Site update'
    git push
}

stack run build-websites

cd .sites
for site in */
do
    cd $site
    siteupdate
    cd ..
done
cd ..

siteupdate