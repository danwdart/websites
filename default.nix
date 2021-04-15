{ 
  nixpkgs ? import <nixpkgs> {},
  unstable ? import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz) {},
  compiler ? "ghc8102", # basement doesn't yet support 901
  ghcjs ? "ghcjs86",
  node ? import ./node-default.nix {} }:
let
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  myHaskellPackages = unstable.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      websites = self.callCabal2nix "websites" (gitignore ./.) {};
      fsutils = self.callCabal2nix "fsutils" (builtins.fetchGit {
        url = "https://github.com/danwdart/fsutils.git";
        rev = "bd85f977a7499a936181a37f4c602bd8b4480d68";
      }) {};
    };
  };
  shell = myHaskellPackages.shellFor {
    packages = p: [
      p.websites
    ];
    buildInputs = with nixpkgs; [
      pkgs.wget
      pkgs.nodejs
      pkgs.docker
      pkgs.awscli2
      haskellPackages.cabal-install
      openssh
      wget
      selenium-server-standalone
      firefox
      geckodriver
      chromedriver
      chromium
      haskellPackages.stack
      haskellPackages.ghcid
      haskellPackages.stylish-haskell
      haskellPackages.hlint
      parallel
    ];
    withHoogle = true;
    inputsFrom = [
      node.shell
    ];
  };
  exe = unstable.haskell.lib.justStaticExecutables (myHaskellPackages.websites);
in
{
  inherit shell;
  inherit exe;
  inherit myHaskellPackages;
  inherit node;
  websites = myHaskellPackages.websites;
}

