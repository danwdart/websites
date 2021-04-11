#!/bin/sh
nix-build shell.nix
nix-store -qR --include-outputs $(nix-instantiate shell.nix) | cachix push websites
