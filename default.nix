{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {}
}:
let
    h = pkgs.haskell.lib;
    hp = pkgs.haskellPackages;
    gitignoreSrc =
        (import sources."gitignore.nix" { inherit (pkgs) lib;}).gitignoreSource;
    misoPkgs = (import sources.miso {}).pkgs.haskell.packages.ghcjs;
in rec {
    usort =
        hp.callCabal2nix
            "usort"
            (gitignoreSrc (pkgs.lib.cleanSource ./usort))
            {};
    usort-web-client =
        misoPkgs.callCabal2nix
            "usort-web"
            (pkgs.lib.cleanSource ./usort-web)
            {};

    shells = {
        ci-shell = pkgs.mkShell {
            buildInputs = [ pkgs.cachix ];
        };
        shell = hp.shellFor {
            packages = _ : [
                usort
            ];
            buildInputs = [
            ];
        };
        client-shell = misoPkgs.shellFor {
            packages = _ : [
                usort-web-client
            ];
            buildInputs = [
            ];
        };
    };
}
