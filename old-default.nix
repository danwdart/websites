{ 
  nixpkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/master.tar.gz") {},
  compiler ? "ghc921"
} :
let
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  myHaskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      websites = self.callCabal2nix "websites" (gitignore ./.) {};
      fsutils = self.callCabal2nix "fsutils" (builtins.fetchGit {
        url = "https://github.com/danwdart/fsutils.git";
        rev = "a1d2c0df9157fa1e5f4a31e4f163bae45d6d08da";
      }) {};
      # Not yet on Hackage
      aeson-diff = self.callCabal2nix "aeson-diff" (builtins.fetchGit {
        url = "https://github.com/thsutton/aeson-diff.git";
        rev = "1d247f4f4a20f528be3ac75982b5927bc779082e";
      }) {};
      # Not in nixpkgs!
      semialign = self.callHackage "semialign" "1.2" {};
      # Not in nixpkgs!
      req = self.callHackage "req" "3.9.1" {};
      # Not in nixpkgs!
      http-client = self.callHackage "http-client" "0.7.8" {};
      # mmark = (self.callHackage "mmark" "0.0.7.2" {}).overrideDerivation(self: {
      #  doCheck = false; # It fails but I don't care as it's nothing I use, just a dependency of a part of a package I don't use.
      # });
    };
  };
  shell = myHaskellPackages.shellFor {
    packages = p: [
      p.websites
    ];
    shellHook = ''
      gen-hie > hie.yaml
      git ls-files src *.nix *.cabal *.hs | xargs krank
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
      krank
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

