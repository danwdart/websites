{ 
  nixpkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/master.tar.gz") {},
  oldnixpkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/e3bda92a1ab0fc6124b0d645890b517e5e492f8f.tar.gz") {},
  compiler ? "ghc901"
} :
let
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
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
      req = nixpkgs.pkgs.haskell.lib.doJailbreak (self.callHackage "req" "3.9.1" {});
      # https://github.com/kallisti-dev/hs-webdriver/issues/177
      webdriver = self.callCabal2nix "webdriver" (builtins.fetchGit {
        url = "https://github.com/danwdart/hs-webdriver.git";
        rev = "52a82be322cbb8ee8e65f87056827a3b89277e2a";
      }) {};
      gogol-core = self.callHackage "gogol-core" "0.5.0" {};
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
      google-chrome
      firefox
      haskellPackages.apply-refact
      haskellPackages.cabal-install
      haskellPackages.ghcid
      haskellPackages.hlint
      haskellPackages.implicit-hie
      haskellPackages.stan
      haskellPackages.stylish-haskell
      haskellPackages.weeder
      oldnixpkgs.selenium-server-standalone
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

