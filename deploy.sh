#!/bin/bash
siteupdate() {
    git add .
    git commit -m 'Site update'
    git push
}

stack run build-websites
cd .sites/dandart
siteupdate
cd ../jolharg
siteupdate
cd ../m0ori
siteupdate
cd ../blog
siteupdate
cd ../..