#!/bin/sh
nix-build shell.nix
nix-store -qR --include-outputs $(nix-instantiate shell.nix --add-root result) | cachix push dandart
nix-store -qR --include-outputs $(nix-instantiate shell.nix --add-root result) | xargs nix-copy-closure --gzip -s --include-outputs dwd@cache.jolharg.com
