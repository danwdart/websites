# Websites

Generates dandart.co.uk, m0ori.co.uk, jolharg.com, blog and review sites.

## Run in Nix (recommended on Linux)

All you should have to do is:

`nix-shell`
## Run in Docker (recomended on non-Linux) - TODO fix bind mounts?

(optional) `docker build -t dandart/websites .`

For the sake of ~/.stack, ~/.ssh, ~/.aws, ~/.serverlessrc amd ~/.serverless, bind-mounting ~ is probably easiest.
Even if you do these individually, you'll have issues sometimes when sls wants to rename something to ~/.serverlessrc.

`docker run -it --rm -v $HOME:$HOME -p8080:8080 -e HOME -w $PWD -v /var/run/docker.sock:/var/run/docker.sock --env-file=.env dandart/websites`

Make sure also:

`npx sls login` and `aws configure` have been run, before doing anything with the Lambdas.