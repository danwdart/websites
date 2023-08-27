{
  nixpkgs ? import <nixpkgs> {},
  haskell-tools ? import (builtins.fetchTarball "https://github.com/danwdart/haskell-tools/archive/master.tar.gz") {
    nixpkgs = nixpkgs;
    compiler = compiler;
  },
  compiler ? "ghc92"
} :
let
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  lib = nixpkgs.pkgs.haskell.lib;
  tools = haskell-tools compiler;
  myHaskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      websites = lib.dontHaddock (self.callCabal2nix "websites" (gitignore ./.) {});
      fsutils = self.callCabal2nix "fsutils" (builtins.fetchGit {
        url = "https://github.com/danwdart/fsutils.git";
        rev = "e5f97a067955afffc8d120249488f9b59c38a24a";
      }) {};
      typst-symbols = self.callCabal2nix "typst-symbols" (nixpkgs.fetchFromGitHub {
        owner = "jgm";
        repo = "typst-symbols";
        rev = "29dcdd8197c1a3b239918062975e8ac7450523ea";
        sha256 = "f73CvkGtUWuNn4JgYSLNXTziXMcFYKnge/HJGaiK2tY=";
      }) {};
      texmath = self.callCabal2nix "texmath" (nixpkgs.fetchFromGitHub {
        owner = "jgm";
        repo = "texmath";
        rev = "05bec1350255f724b8c449041ec37875fb774329";
        sha256 = "CY0SLY3DkjmfmdZ4dADbpYypKZIon1GrFGbASoxw5Z4=";
      }) {};
      # not released yet
      #req = self.callHackage "req" "3.13.0" {};
      # feed = lib.doJailbreak super.feed;

      # 2.17.1.1: aeson >=0.7 && <2.1
      # pandoc = lib.doJailbreak super.pandoc;

      # 0.0.4: text >=1.2 && <1.3
      # string-qq = lib.doJailbreak super.string-qq;

      # 2.1.0: aeson >=1.5 && <2.1
      # hslua-aeson = lib.doJailbreak super.hslua-aeson;

      #aeson-diff = self.callCabal2nix "aeson-diff" (builtins.fetchGit {
      #  url = "https://github.com/ysangkok/aeson-diff.git";
      #  rev = "5f2051042fb350ecd1bb63379cc7e2c768fe494f";
      #}) {};

      # http-api-data = lib.doJailbreak super.http-api-data;
      # servant = lib.doJailbreak super.servant;
      # servant-server = lib.doJailbreak super.servant-server;
    };
  };
  shell = myHaskellPackages.shellFor {
    packages = p: [
      p.websites
    ];
    shellHook = ''
      gen-hie > hie.yaml
      for i in $(find -type f | grep -v dist-newstyle); do krank $i; done
      doctest src lib
      cabal update
    '';
    buildInputs = tools.defaultBuildTools;
    withHoogle = false;
  };
  exe = lib.justStaticExecutables (myHaskellPackages.websites);
in
{
  inherit shell;
  websites = lib.justStaticExecutables (myHaskellPackages.websites);
}

