{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc884" }:
let
  myHaskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      fsutils = self.callCabal2nix "fsutils" (builtins.fetchGit {
        url = "https://github.com/danwdart/fsutils.git";
        rev = "bd85f977a7499a936181a37f4c602bd8b4480d68";
      }) {};
      serverless-haskell = self.callCabal2nix "serverless-haskell" (builtins.fetchGit {
        url = "https://github.com/seek-oss/serverless-haskell.git";
        rev = "249611a6f82f500691fbd55035b5ddb262a2962d";
      }) {};
    };
  };
in
myHaskellPackages.callPackage ./websites.nix {}
