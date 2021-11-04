{ 
  nixpkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/master.tar.gz") {},
  oldnixpkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/e3bda92a1ab0fc6124b0d645890b517e5e492f8f.tar.gz") {},
  compiler ? "ghc921"
} :
let
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  lib = nixpkgs.pkgs.haskell.lib;
  myHaskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      websites = self.callCabal2nix "websites" (gitignore ./.) {};
      fsutils = lib.doJailbreak (self.callCabal2nix "fsutils" (builtins.fetchGit {
        url = "https://github.com/danwdart/fsutils.git";
        rev = "324369ee5ff5d2c797b5d00d55e24e74d631c40f";
      }) {});
      # Changes needed for 9.0.1
      # Not yet pushed to hackage
      aeson-diff = (lib.doJailbreak (self.callCabal2nix "aeson-diff" (builtins.fetchGit {
        url = "https://github.com/thsutton/aeson-diff.git";
        rev = "1d247f4f4a20f528be3ac75982b5927bc779082e";
      }) {})).overrideDerivation(g: {
        doCheck = false;
      });
      # not in nix
      semialign = lib.doJailbreak (
        self.callCabal2nixWithOptions "semialign" (builtins.fetchGit {
          url = "https://github.com/haskellari/these.git";
          rev = "6897306f3d87aa8abd45cacaa3b24f5ab1f045a5";
        }) "--subpath semialign" {}
      );
      # Depends on cabal-un-published http-client versions.
      req = lib.doJailbreak (self.callHackage "req" "3.9.1" {});
      # Changes needed for 9.2.1
      JuicyPixels = lib.doJailbreak (super.JuicyPixels);
      blaze-markup = lib.doJailbreak (super.blaze-markup);
      xml-conduit = self.callCabal2nixWithOptions "" (builtins.fetchGit {
        url = "https://github.com/snoyberg/xml.git";
        rev = "417eea280a9e7fd0e356489afd31d07f4f64487a";
      }) "--subpath xml-conduit" {};
      charset = lib.doJailbreak (self.callCabal2nix "charset" (builtins.fetchGit {
        url = "https://github.com/ekmett/charset.git";
        rev = "4f456a30513212bcb7c7ec14ab25880a43a3aa18";
      }) {});
      websockets = lib.doJailbreak (super.websockets);
      hslua = lib.doJailbreak (super.hslua);
      iproute = lib.doJailbreak (self.callCabal2nix "iproute" (builtins.fetchGit {
        url = "https://github.com/kazu-yamamoto/iproute.git";
        rev = "f387a37e07a4074d8e2b912834d93b6628d0befd";
      }) {});
      network-byte-order = (lib.doJailbreak (self.callCabal2nix "network-byte-order" (builtins.fetchGit {
        url = "https://github.com/kazu-yamamoto/network-byte-order.git";
        rev = "3570bb3a1aeae36c241bbad97f0c4d22162a2a65";
      }) {})).overrideDerivation(g: {
        doCheck = false; # {..} without the extension enabled
      });
      modern-uri = lib.doJailbreak (super.modern-uri);
      prettyprinter-ansi-terminal = super.prettyprinter-ansi-terminal.overrideDerivation(g: {
        doCheck = false; # confuses String with Text
      });
      # https://github.com/well-typed/generics-sop/pull/147
      sop-core = (lib.doJailbreak (self.callCabal2nixWithOptions "sop-core" (builtins.fetchGit {
        url = "https://github.com/danwdart/generics-sop.git";
        rev = "39386037d49bd21871a6479eaba9f4d5f4dbdf10";
      }) "--subpath sop-core" {}));
      generics-sop = lib.doJailbreak (self.callCabal2nixWithOptions "generics-sop" (builtins.fetchGit {
        url = "https://github.com/danwdart/generics-sop.git";
        rev = "39386037d49bd21871a6479eaba9f4d5f4dbdf10";
      }) "--subpath generics-sop" {});
      vault = lib.doJailbreak (super.vault);
      unicode-transforms = lib.doJailbreak (super.unicode-transforms);
      # https://github.com/haskellari/tree-diff/pull/59
      tree-diff = lib.doJailbreak (self.callCabal2nix "tree-diff" (builtins.fetchGit {
        url = "https://github.com/danwdart/tree-diff.git";
        rev = "be5617cd12c8e2dbb08f1fceddcf8d3a59f4cf43";
      }) {});
      # https://github.com/haskellari/cabal-doctest/pull/71
      cabal-doctest = lib.doJailbreak (self.callCabal2nix "cabal-doctest" (builtins.fetchGit {
        url = "https://github.com/AlistairB/cabal-doctest.git";
        ref = "cabal-3-6";
      }) {});
    };
  };
  shell = myHaskellPackages.shellFor {
    packages = p: [
      p.websites
    ];
    shellHook = ''
      gen-hie > hie.yaml
      for i in $(find -type f); do krank $i; done
    '';
    buildInputs = with nixpkgs; with haskellPackages; [
      apply-refact
      cabal-install
      ghcid
      hlint
      implicit-hie
      krank
      stan
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

