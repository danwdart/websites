{ 
  nixpkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/master.tar.gz") {},
  compiler ? "ghc901",
} :
let
  crossPkgs = nixpkgs.pkgsCross.aarch64-multiplatform;
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  myHaskellPackages = crossPkgs.pkgs.haskell.packages.${compiler}.override {
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
      req = nixpkgs.pkgs.haskell.lib.doJailbreak (self.callHackage "req" "3.9.1" {});
    };
  };
  shell = myHaskellPackages.shellFor {
    packages = p: [
      p.websites
    ];
    buildInputs = with nixpkgs; [
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
