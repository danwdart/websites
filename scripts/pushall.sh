#!/bin/sh
nix-build shell.nix
nix-store -qR --include-outputs $(nix-instantiate shell.nix --add-root result) | cachix push websites
