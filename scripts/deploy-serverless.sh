#!/usr/bin/env nix-shell
#! nix-shell -i bash
doctl k c c \
    --1-clicks openfaas z
    --node-pool "name=openfaas;size=s-1vcpu-2gb;count=1" \
    --region lon1 \
    openfaas

# doctl k c cfg show X
# doctl k c cfg save X
# faas-cli new websites --lang=dockerfile
# docker build...
# mv websites.yml stack.yml
# rm -rf build
# faas-cli up -f websites.yml --image dandart/websites-openfaas