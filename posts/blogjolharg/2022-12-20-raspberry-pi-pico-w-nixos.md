---
title: 'Raspberry Pi Pico (W) on NixOS'
date: 2022-12-20T19:16:15Z
draft: false
aliases: [ "/2022/12/raspberry-pi-pico-w-nixos" ]
tags: [raspberry, pi, pico, w, nixos]
---

If you're following [my GitHub](https://github.com/danwdart), you'll know I've got a new testing project for the Raspberry Pi Pico (W).

Here's the `shell.nix` file again:

```nix
with import <nixpkgs> {};
runCommand "pico" {
    shellHook = ''
      export PICO_SDK_FETCH_FROM_GIT=on
      export PICO_EXTRAS_FETCH_FROM_GIT=on
      export BOARD=pico-w
      export PICO_BOARD=pico_w
    '';
    buildInputs = [
      cmake
      gcc
      gcc-arm-embedded-10
      gnumake
      pico-sdk
      picotool
      python311
    ];
} ""

```

Putting this in the root of a directory allows one to compile Pico (W) projects, avoiding polluting the global environment.

Huzzah!

