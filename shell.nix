(import ./default.nix).shellFor {
    withHoogle = false;
    # apply-refact
    tools = {
        cabal = "latest";
        hlint = "latest";
        # cabal-install
        ghcid = "latest";
        # implicit-hie = "latest";
        # stan = "latest";
        stylish-haskell = "latest";
        # weeder = "latest";
        krank = "latest";
        # openssh = "latest";
        # parallel = "latest";
        # wget = "latest";
    };
}
