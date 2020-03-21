#!/bin/bash
siteupdate() {
    git add .
    git commit -m 'Site update'
    git push
}

stack build
stack run dandart
stack run jolharg
cd .sites/dandart
siteupdate
cd ../jolharg
siteupdate
cd ../..