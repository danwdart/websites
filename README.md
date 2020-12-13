# Websites

Generates dandart.co.uk, m0ori.co.uk and jolharg.com

## Run in Nix (recommended on Linux)

All should be OK.

`nix-shell`
## Run in Docker (recomended on non-Linux) - TODO fix bind mounts?

(optional) `docker build -t dandart/websites .`

`docker run -it --rm -v $PWD:/app -v /var/run/docker.sock:/var/run/docker.sock -w $PWD dandart/websites`