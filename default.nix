{ 
  nixpkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/master.tar.gz") {},
  compiler ? "ghc921"
} :
let
  crossPkgs = nixpkgs.pkgsCross.aarch64-multiplatform;
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  lib = nixpkgs.pkgs.haskell.lib;
  myHaskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      websites = self.callCabal2nix "websites" (gitignore ./.) {};
      fsutils = self.callCabal2nix "fsutils" (builtins.fetchGit {
        url = "https://github.com/danwdart/fsutils.git";
        rev = "324369ee5ff5d2c797b5d00d55e24e74d631c40f";
      }) {};
      # Changes needed for 9.0.1
      # Not yet pushed to hackage
      aeson-diff = (self.callCabal2nix "aeson-diff" (builtins.fetchGit {
        url = "https://github.com/thsutton/aeson-diff.git";
        rev = "1d247f4f4a20f528be3ac75982b5927bc779082e";
      }) {});
      # not in nix
      semialign = self.callHackage "semialign" "1.2" {};
      # Depends on cabal-un-published http-client versions.
      req = lib.doJailbreak (self.callHackage "req" "3.9.2" {});
      webdriver = self.callCabal2nix "webdriver" (builtins.fetchGit {
        url = "https://github.com/danwdart/hs-webdriver.git";
        rev = "a37d3a28d88374416b38ed37edbc304e44b66268";
      }) {};
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
    buildInputs = with myHaskellPackages; with nixpkgs; with haskellPackages; [
      apply-refact
      cabal-install
      doctest
      ghci-dap
      ghcid
      ghcide
      haskell-dap
      haskell-debug-adapter
      haskell-language-server
      hasktags
      hlint
      implicit-hie
      krank
      stan
      nixpkgs.selenium-server-standalone # http-conduit-downloader is marked as broken
      stylish-haskell
      weeder
    ];
    withHoogle = false;
  };
  exe = lib.justStaticExecutables (myHaskellPackages.websites);
in
{
  inherit shell;
  inherit exe;
  inherit myHaskellPackages;
  websites = myHaskellPackages.websites;
}

