# Websites

Generates dandart.co.uk, m0ori.co.uk and jolharg.com

## Run in Nix (recommended on Linux)

All should be OK.

`nix-shell`
## Run in Docker (recomended on non-Linux) - TODO fix bind mounts?

(optional) `docker build -t dandart/websites .`

`docker run -it --rm -v ~/.ssh:/root/.ssh -v ~/.stack:/root/.stack -v $PWD:/app -v /var/run/docker.sock:/var/run/docker.sock dandart/websites`

`docker run -it --rm -v ~/.ssh:/root/.ssh -v ~/.stack:/root/.stack -e HOME -v $PWD:$PWD -w $PWD -v /var/run/docker.sock:/var/run/docker.sock dandart/websites`


Then inside, because of Docker..

```shell
stack --docker --docker-image=fpco/stack-build:lts-13.30 ls dependencies
```