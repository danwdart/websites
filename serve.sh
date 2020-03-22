#!/bin/bash
trap "kill 0" EXIT
warp -h dandart.localhost -d .sites/dandart &
echo "http://dandart.localhost:3000/"
warp -h jolharg.localhost -d  .sites/jolharg &
echo "http://jolharg.localhost:3000/"
warp -h m0ori.localhost -d  .sites/m0ori &
echo "http://m0ori.localhost:3000/"
wait