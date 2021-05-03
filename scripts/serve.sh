#!/usr/bin/env nix-shell
#! nix-shell -p haskellPackages.wai-app-static -i bash
for SITE in .sites/*
do
    BASE=$(basename $SITE)
    echo Hosting $BASE on http://$BASE.localhost:3000 ...
    warp -h $BASE.localhost -d $SITE
done
wait