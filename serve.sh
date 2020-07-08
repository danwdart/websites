#!/bin/bash
trap "kill 0" EXIT
warp -h dandart.localhost -d .sites/dandart -p 8080 &
echo "http://dandart.localhost:8080/"
warp -h jolharg.localhost -d .sites/jolharg -p 8081 &
echo "http://jolharg.localhost:8081/"
warp -h m0ori.localhost -d .sites/m0ori -p 8082 &
echo "http://m0ori.localhost:8082/"
warp -h blog.localhost -d .sites/blog -p 8083 &
echo "http://blog.localhost:8083/"
wait