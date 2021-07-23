{ 
  nixpkgs ? import <nixpkgs> {},
  unstable ? import <unstable> {},
  compiler ? "ghc8104", # basement doesn't yet support 901
  ghcjs ? "ghcjs86"
} :
let
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  myHaskellPackages = unstable.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      websites = self.callCabal2nix "websites" (gitignore ./.) {};
      fsutils = self.callCabal2nix "fsutils" (builtins.fetchGit {
        url = "https://github.com/danwdart/fsutils.git";
        rev = "bd85f977a7499a936181a37f4c602bd8b4480d68";
      }) {};
      mmark = (self.callHackage "mmark" "0.0.7.2" {}).overrideDerivation(self: {
        doCheck = false; # It fails but I don't care as it's nothing I use, just a dependency of a part of a package I don't use.
      });
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
    withHoogle = true;
  };
  exe = unstable.haskell.lib.justStaticExecutables (myHaskellPackages.websites);
in
{
  inherit shell;
  inherit exe;
  inherit myHaskellPackages;
  websites = myHaskellPackages.websites;
}

