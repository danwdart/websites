# Websites

Generates dandart.co.uk, m0ori.co.uk, jolharg.com, blog and review sites.

## Run in Nix (recommended on Linux)

### Caches

Include:
```conf
substituters = https://websites.cachix.org https://hydra.iohk.io
trusted-public-keys = websites.cachix.org-1:YMPYgEeWohlGq/0wDvWLVSRoNcBS1jIOmku6Djv7zcM= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
```

into your `~/.config/nix/nix.conf`.

All you should have to do then is:

`nix-shell`

Includes an `.envrc` so if you like you can just `direnv allow` and you'll switch into the correct environment when you enter the directory.