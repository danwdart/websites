{ 
  nixpkgs ? import <nixpkgs> {},
  haskell-tools ? import (builtins.fetchTarball "https://github.com/danwdart/haskell-tools/archive/master.tar.gz") {
    nixpkgs = nixpkgs;
    compiler = compiler;
  },
  compiler ? "ghc924"
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
      # Changes needed for 9.0.1
      # Not yet pushed to hackage
      #aeson-diff = (self.callCabal2nix "aeson-diff" (builtins.fetchGit {
      #  url = "https://github.com/ysangkok/aeson-diff.git";
      #  rev = "37101dc86af1be9ba16a45040998ae920f524010";
      #}) {});
      # not in nix
      #semialign = self.callHackage "semialign" "1.2.0.1" {};
      #webdriver = self.callCabal2nix "webdriver" (builtins.fetchGit {
      #  url = "https://github.com/danwdart/hs-webdriver.git";
      #  rev = "a37d3a28d88374416b38ed37edbc304e44b66268";
      #}) {};
      #http-conduit-downloader = self.callHackage "http-conduit-downloader" "1.1.4" {};
      #clay = lib.doJailbreak super.clay;
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

