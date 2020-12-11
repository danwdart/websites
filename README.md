# Websites

Generates dandart.co.uk, m0ori.co.uk and jolharg.com

TODO:

Dead link finder
Tracker

## Run in Nix

All should be OK.

`nix-shell`

## Run in Docker

(optional) `docker build -t dandart/websites .`

`docker run -it --rm -v $PWD:/app -v /var/run/docker.sock:/var/run/docker.sock -w /app dandart/websites nix-shell`