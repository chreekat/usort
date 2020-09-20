{ sources ? import ./nix/sources.nix
, compiler ? "ghc"
}:
let
    miso = import sources.miso {};
    pkgs = miso.pkgs;
    h = pkgs.haskell.lib;
    ghcPkgs = pkgs.haskell.packages.ghc865;
    ghcjsPkgs = pkgs.haskell.packages.ghcjs86;
    gitignoreSrc =
        (import sources."gitignore.nix" {}).gitignoreSource;
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
        usort-lib = ghcPkgs.shellFor {
          packages = p: [ (usort-lib p) ];
          withHoogle = true;
        };
    };
}
