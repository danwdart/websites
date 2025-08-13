{
  nixpkgs ? import <nixpkgs> {},
  haskell-tools ? import (builtins.fetchTarball "https://github.com/danwdart/haskell-tools/archive/master.tar.gz") {
    inherit nixpkgs;
    inherit compiler;
  },
  compiler ? "ghc912"
} :
let
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  inherit (nixpkgs.pkgs.haskell) lib;
  tools = haskell-tools compiler;
  myHaskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      # dontCheck because it requires a browser, selenium and a writable home directory...
      websites = lib.dontCheck (lib.dontHaddock (self.callCabal2nix "websites" (gitignore ./.) {}));
      # https://github.com/haskell-party/feed/issues/73
      # https://github.com/haskell/security-advisories/pull/220
      # feed = lib.doJailbreak super.feed;
      # toml-parser = 
      # toml-parser = lib.doJailbreak super.toml-parser;
      # citeproc = lib.doJailbreak super.citeproc;

      # not yet in nix
      pandoc = self.callHackageDirect {
        pkg = "pandoc";
        ver = "3.6.2";
        sha256 = "sha256-b4CaNo35Oo1U3brgvgqLQzWUQ4cp9jop27n5DwAJWu8=";
      } {};

      # pandoc = lib.doJailbreak super.pandoc;
      # # not yet here
      # text = self.callHackage "text" "2.1.2" {};
      # # older version requires older text
      # # also fails tests for some reason
      # parsec = lib.dontCheck (self.callHackageDirect {
      #   pkg = "parsec";
      #   ver = "3.1.18.0";
      #   sha256 = "FhFYDHqwEFdr3NBWcdWffLUpNGGA5PeEMtzE3tNIMiE=";
      # } {});
      # Cabal-syntax = lib.doJailbreak super.Cabal-syntax;
      # dotenv = self.callHackage "dotenv" "0.12.0.0" {};
    };
  };
  shell = myHaskellPackages.shellFor {
    packages = p: [
      p.websites
    ];
    shellHook = ''
      gen-hie > hie.yaml
      for i in $(find . -type f | grep -v "dist-*"); do krank $i; done
      doctest src lib
      cabal update
    '';
    buildInputs = tools.defaultBuildTools ++ [ nixpkgs.openjdk17-bootstrap ];
    withHoogle = false;
  };
  in
{
  inherit shell;
  websites = lib.justStaticExecutables myHaskellPackages.websites;
}

