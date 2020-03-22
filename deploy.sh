#!/bin/bash
siteupdate() {
    git add .
    git commit -m 'Site update'
    git push
}

./build.sh
cd .sites/dandart
siteupdate
cd ../jolharg
siteupdate
cd ../m0ori
siteupdate
cd ../..