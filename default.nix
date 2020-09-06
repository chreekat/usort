{ sources ? import ./nix/sources.nix
, compiler ? "ghc"
}:
let
    pkgs = import sources.nixpkgs {};
    miso = import sources.miso {};
    h = pkgs.haskell.lib;
    ghcPkgs = pkgs.haskellPackages;
    ghcjsPkgs = miso.pkgs.haskell.packages.ghcjs;
    gitignoreSrc =
        (import sources."gitignore.nix" { inherit (pkgs) lib;}).gitignoreSource;
    usort-lib = eitherPkgs:
        eitherPkgs.callCabal2nix
            "usort-lib"
            (gitignoreSrc (pkgs.lib.cleanSource ./usort-lib))
            {};
in rec {

    usort-console =
        ghcPkgs.callCabal2nix
            "usort-console"
            (gitignoreSrc (pkgs.lib.cleanSource ./usort-console))
            { usort-lib = usort-lib ghcPkgs; };

    usort-web-client =
        ghcjsPkgs.callCabal2nix
            "usort-web"
            (pkgs.lib.cleanSource ./usort-web)
            { usort-lib = usort-lib ghcjsPkgs; };

    shells = {
        ci = pkgs.mkShell {
            buildInputs = [ pkgs.cachix ];
        };
    };
}
