{ 
  nixpkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/master.tar.gz") {},
  compiler ? "ghc921"
} :
let
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  lib = nixpkgs.pkgs.haskell.lib;
  hsluaRepository = builtins.fetchGit {
    url = "https://github.com/hslua/hslua.git";
    rev = "0890414e05938b1652e0f4c8382cc4776d2f8708";
  };
  myHaskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      websites = self.callCabal2nix "websites" (gitignore ./.) {};
      fsutils = lib.doJailbreak (self.callCabal2nix "fsutils" (builtins.fetchGit {
        url = "https://github.com/danwdart/fsutils.git";
        rev = "324369ee5ff5d2c797b5d00d55e24e74d631c40f";
      }) {});
      # Changes needed for 9.0.1
      # Not yet pushed to hackage
      aeson-diff = lib.dontCheck (lib.doJailbreak (self.callCabal2nix "aeson-diff" (builtins.fetchGit {
        url = "https://github.com/thsutton/aeson-diff.git";
        rev = "1d247f4f4a20f528be3ac75982b5927bc779082e";
      }) {}));
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
      JuicyPixels = lib.doJailbreak super.JuicyPixels;
      blaze-markup = lib.doJailbreak super.blaze-markup;
      xml-conduit = lib.dontCheck (self.callCabal2nixWithOptions "" (builtins.fetchGit {
        url = "https://github.com/snoyberg/xml.git";
        rev = "417eea280a9e7fd0e356489afd31d07f4f64487a";
      }) "--subpath xml-conduit" {});
      charset = lib.doJailbreak (self.callCabal2nix "charset" (builtins.fetchGit {
        url = "https://github.com/ekmett/charset.git";
        rev = "4f456a30513212bcb7c7ec14ab25880a43a3aa18";
      }) {});
      websockets = lib.doJailbreak (super.websockets);
      # hslua = lib.doJailbreak (super.hslua);
      iproute = lib.doJailbreak (self.callCabal2nix "iproute" (builtins.fetchGit {
        url = "https://github.com/kazu-yamamoto/iproute.git";
        rev = "f387a37e07a4074d8e2b912834d93b6628d0befd";
      }) {});
      network-byte-order = lib.dontCheck (lib.doJailbreak (self.callCabal2nix "network-byte-order" (builtins.fetchGit {
        url = "https://github.com/kazu-yamamoto/network-byte-order.git";
        rev = "3570bb3a1aeae36c241bbad97f0c4d22162a2a65";
      }) {}));
      modern-uri = lib.doJailbreak super.modern-uri;
      prettyprinter-ansi-terminal = lib.dontCheck super.prettyprinter-ansi-terminal;
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
      cabal-doctest = lib.doJailbreak (self.callCabal2nix "cabal-doctest" (builtins.fetchGit {
        url = "https://github.com/haskellari/cabal-doctest.git";
        rev = "2338f86cbba06366fca42f7c9640bc408c940e0b";
      }) {});
      wai-logger = lib.doJailbreak super.wai-logger;
      cereal = lib.doJailbreak (super.cereal);
      unicode-collation = lib.doJailbreak super.unicode-collation;
      # tried to use 0.2.1.2
      # 0.2.2.1 isn't found in hackage archive
      commonmark-extensions = self.callCabal2nixWithOptions "commonmark-extensions" (builtins.fetchGit {
        url = "https://github.com/jgm/commonmark-hs.git";
        rev = "3e7539a6636a2168d1ef727be8bd995ae05d3de0";
      }) "--subpath commonmark-extensions" {};
      hedgehog = lib.doJailbreak super.hedgehog;
      tasty-hedgehog = lib.doJailbreak super.tasty-hedgehog;
      bsb-http-chunked = lib.dontCheck super.bsb-http-chunked;
      warp = lib.dontCheck (self.callCabal2nixWithOptions "warp" (builtins.fetchGit {
        url = "https://github.com/danwdart/wai.git";
        rev = "397a32a6c7927c15e15468fbc63a1ca640491bff";
      }) "--subpath warp" {});
      # Can't pull off of Hackage (not found in archive)...
      reducers = self.callCabal2nix "reducers" (builtins.fetchGit {
        url = "https://github.com/ekmett/reducers.git";
        rev = "6ca275669539d348a75d3807eb964d8936a9ae46";
      }) {};
      feed = lib.doJailbreak super.feed;
      # Needs to be at least 2.16 to support GHC 9.2.
      pandoc = lib.doJailbreak (self.callCabal2nix "pandoc" (builtins.fetchGit {
        url = "https://github.com/jgm/pandoc.git";
        rev = "7a70a46c0319f279fdee3926abff08922be2f02c";
      }) {});
      hslua-module-version = lib.dontCheck ((
        lib.markUnbroken (lib.doJailbreak (
          self.callCabal2nixWithOptions "hslua-module-version" hsluaRepository "--subpath hslua-module-version" {}
        ))
      ).overrideDerivation (self: {
        prePatch = ''
          sed -i 's/, hslua .*/, hslua, hslua-core/g' hslua-module-version.cabal
        '';
      }));
      lpeg = self.callCabal2nixWithOptions "lpeg" hsluaRepository "--subpath lpeg" {};
      hslua = lib.doJailbreak super.hslua;

      # which makes the older version of this broken...
      #hslua-module-version = lib.doJailbreak (
      #  self.callCabal2nixWithOptions "hslua-module-version" hsluaRepository "--subpath hslua-module-version" {}
      #);
      #hslua-module-path = lib.doJailbreak (
      #  self.callCabal2nixWithOptions "hslua-module-path" hsluaRepository "--subpath hslua-module-path" {}
      #);
      #hslua = lib.doJailbreak (
      #  self.callCabal2nixWithOptions "hslua" hsluaRepository "--subpath hslua" {}
      #);
      #hslua-core = lib.doJailbreak (
      #  self.callCabal2nixWithOptions "hslua-core" hsluaRepository "--subpath hslua-core" {}
      #);
      #lua = lib.doJailbreak (
      #  self.callCabal2nixWithOptions "lua" hsluaRepository "--subpath lua" {}
      #);

      # @TODO ensure latest version? The above requires an older hslua.
      # hslua = lib.doJailbreak (self.callHackage "hslua" "2.0.1" {});
      # super.hslua;
      #  self.callCabal2nixWithOptions "hslua" (builtins.fetchGit {
      #    url = "https://github.com/hslua/hslua.git";
      #    rev = "0890414e05938b1652e0f4c8382cc4776d2f8708";
      #  }) "--subpath hslua" {
      #    lua = nixpkgs.lua;
      #  }
      #);
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
    buildInputs = with myHaskellPackages; with nixpkgs; with haskellPackages; [
      apply-refact
      cabal-install
      ghcid
      haskell-language-server
      hasktags
      hlint
      implicit-hie
      krank
      stan
      stylish-haskell
      haskellPackages.weeder # not on ghc 9.2 because of generic-lens-core issues
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

