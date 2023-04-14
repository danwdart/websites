# Websites

Generates dandart.co.uk, m0ori.co.uk, jolharg.com, blog and review sites.

## Run in Nix (recommended on Linux)

All you should have to do is:

`nix-shell`

Includes an `.envrc` so if you like you can just `direnv allow` and you'll switch into the correct environment when you enter the directory.

### Filesystem warning

This repository will not clone properly when trying to clone into FAT, HFS, Joliet/CDFS, MFS, NTFS (Win32NS) or ReFS (Win32NS) filesystems, due to the use of their restrictions making them being incompatible with certain characters in filenames.

It is also not recommended to clone into other filesystems not supporting permissions such as ExFAT.