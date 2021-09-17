{ 
  nixpkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/master.tar.gz") {},
  compiler ? "ghc901"
} :
let
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  myHaskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      websites = self.callCabal2nix "websites" (gitignore ./.) {};
      fsutils = nixpkgs.pkgs.haskell.lib.doJailbreak(self.callCabal2nix "fsutils" (builtins.fetchGit {
        url = "https://github.com/danwdart/fsutils.git";
        rev = "bd85f977a7499a936181a37f4c602bd8b4480d68";
      }) {});
      # Changes needed for 9.0.1
      aeson-diff = nixpkgs.pkgs.haskell.lib.doJailbreak super.aeson-diff;
      semialign = nixpkgs.pkgs.haskell.lib.doJailbreak super.semialign;
      req = nixpkgs.pkgs.haskell.lib.doJailbreak (self.callHackage "req" "3.9.1" {});
    };
  };
  shell = myHaskellPackages.shellFor {
    packages = p: [
      p.websites
    ];
    shellHook = ''
      gen-hie > hie.yaml
    '';
    buildInputs = with nixpkgs; [
      haskellPackages.apply-refact
      haskellPackages.cabal-install
      haskellPackages.ghcid
      haskellPackages.hlint
      haskellPackages.implicit-hie
      haskellPackages.stan
      haskellPackages.stylish-haskell
      haskellPackages.weeder
      openssh
      parallel
      wget
    ];
    withHoogle = false;
  };
  exe = nixpkgs.haskell.lib.justStaticExecutables (myHaskellPackages.websites);
in
{
  inherit shell;
  inherit exe;
  inherit myHaskellPackages;
  websites = myHaskellPackages.websites;
}

