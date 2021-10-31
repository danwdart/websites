#!/usr/bin/env nix-shell
#! nix-shell -p inotify-tools -i bash

frontend() {
    pkill nix-build
    nix-build
    result/bin/serve
}

frontend &

(while inotifywait -qe close_write -r websites.cabal lib src posts reviews static
do
    frontend &
done) &

wait