#!/bin/bash
siteupdate() {
    git add .
    git commit -m 'Site update'
    git push
}

stack run
cd .sites/dandart
siteupdate
cd ../jolharg
siteupdate
cd ../m0ori
siteupdate
cd ../..