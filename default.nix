{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {}
}:
let
    h = pkgs.haskell.lib;
    hp = pkgs.haskellPackages;
    gitignoreSrc =
        (import sources."gitignore.nix" { inherit (pkgs) lib;}).gitignoreSource;
    usort =
        hp.callCabal2nix
            "usort"
            (gitignoreSrc (pkgs.lib.cleanSource ./usort))
            {};
    usort-it-client =
        pkgs.haskell.packages.ghcjs.callCabal2nix
            "usort.it"
            (pkgs.lib.cleanSource ./usort.it)
            {};

in {
    inherit usort usort-it-client;
    shell = hp.shellFor {
        packages = _ : [
            usort
        ];
        buildInputs = [
        ];
    };
}
