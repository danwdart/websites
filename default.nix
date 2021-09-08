let sources = {
        haskellNix = builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz";
    };
    haskellNix = import sources.haskellNix {};
    pkgs = import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs;
in pkgs.haskell-nix.project {
    src = ./.;
    compiler-nix-name = "ghc901";
}