{ 
  nixpkgs ? import <nixpkgs> {},
  unstable ? import (fetchTarball https://github.com/NixOS/nixpkgs/archive/master.tar.gz) {},
  compiler ? "ghc8104", # basement doesn't yet support 901
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
      mmark = (self.callHackage "mmark" "0.0.7.2" {}).overrideDerivation(self: {
        doCheck = false; # It fails but I don't care as it's nothing I use, just a dependency of a part of a package I don't use.
      });
      # https://github.com/brendanhay/gogol/issues/148
      gogol-core = self.callCabal2nixWithOptions "gogol-core" (builtins.fetchGit { # not yet released
        url = "https://github.com/brendanhay/gogol";
        rev = "d7c7d71fc985cd96fb5f05173da6c607da362b74";
      }) "--subpath core" {};
    };
  };
  shell = myHaskellPackages.shellFor {
    packages = p: [
      p.websites
    ];
    buildInputs = [
      nixpkgs.pkgs.wget
      nixpkgs.pkgs.nodejs
      nixpkgs.pkgs.docker
      nixpkgs.pkgs.awscli2
      nixpkgs.haskellPackages.cabal-install
      nixpkgs.openssh
      nixpkgs.wget
      nixpkgs.selenium-server-standalone
      nixpkgs.firefox
      nixpkgs.geckodriver
      nixpkgs.chromedriver
      nixpkgs.chromium
      nixpkgs.haskellPackages.stack
      nixpkgs.haskellPackages.ghcid
      nixpkgs.haskellPackages.stylish-haskell
      nixpkgs.haskellPackages.hlint
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

