{
  nixpkgs ? import <nixpkgs> {},
  haskell-tools ? import (builtins.fetchTarball "https://github.com/danwdart/haskell-tools/archive/master.tar.gz") {
    nixpkgs = nixpkgs;
    compiler = compiler;
  },
  compiler ? "ghc910"
} :
let
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  lib = nixpkgs.pkgs.haskell.lib;
  tools = haskell-tools compiler;
  myHaskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      # dontCheck because it requires a browser, selenium and a writable home directory...
      websites = lib.dontCheck (lib.dontHaddock (self.callCabal2nix "websites" (gitignore ./.) {}));
      fsutils = lib.doJailbreak (self.callCabal2nix "fsutils" (builtins.fetchGit {
        url = "https://github.com/danwdart/fsutils.git";
        rev = "e5f97a067955afffc8d120249488f9b59c38a24a";
      }) {});
      feed = lib.doJailbreak super.feed;
      toml-parser = lib.doJailbreak super.toml-parser;
      citeproc = lib.doJailbreak super.citeproc;
      pandoc = lib.doJailbreak super.pandoc;
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
    buildInputs = tools.defaultBuildTools ++ [ nixpkgs.openjdk17-bootstrap ];
    withHoogle = false;
  };
  in
{
  inherit shell;
  websites = lib.justStaticExecutables (myHaskellPackages.websites);
}

