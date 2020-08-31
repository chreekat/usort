{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {}
}:
let
    h = pkgs.haskell.lib;
    hp = pkgs.haskellPackages;
    gitignoreSrc =
        (import sources."gitignore.nix" { inherit (pkgs) lib;}).gitignoreSource;
    miso = (import sources.miso {});
    usort =
        hp.callCabal2nix
            "usort"
            (gitignoreSrc (pkgs.lib.cleanSource ./usort))
            {};
    usort-web-client =
        miso.pkgs.haskell.packages.ghcjs.callCabal2nix
            "usort-web"
            (pkgs.lib.cleanSource ./usort-web)
            {};

in {
    inherit usort usort-web-client;
    shell = hp.shellFor {
        packages = _ : [
            usort
        ];
        buildInputs = [
        ];
    };
    client-shell = miso.pkgs.haskell.packages.ghcjs.shellFor {
        packages = _ : [
            usort-web-client
        ];
        buildInputs = [
        ];
    };
}
