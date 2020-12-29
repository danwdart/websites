{ nixpkgs ? import <nixpkgs> {},
  compiler ? "ghc884",
  ghcjs ? "ghcjs884",
  node ? import ./node-default.nix {} }:
let
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  myHaskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      websites = self.callCabal2nix "websites" (gitignore ./.) {};
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
  exe = nixpkgs.haskell.lib.justStaticExecutables (myHaskellPackages.websites);
in
{
  inherit shell;
  inherit exe;
  inherit myHaskellPackages;
  inherit node;
  websites = myHaskellPackages.websites;
}