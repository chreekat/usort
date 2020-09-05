{ sources ? import ./nix/sources.nix
, compiler ? "ghc"
}:
let
    pkgs = import sources.nixpkgs {};
    miso = import sources.miso {};
    h = pkgs.haskell.lib;
    ghcPkgs = pkgs.haskellPackages;
    ghcjsPkgs = miso.pkgs.haskell.packages.ghcjs;
    eitherPkgs =
      if compiler == "ghc"
        then ghcPkgs
        else ghcjsPkgs;
    gitignoreSrc =
        (import sources."gitignore.nix" { inherit (pkgs) lib;}).gitignoreSource;
in rec {
    # Usort for server
    usort =
        eitherPkgs.callCabal2nix
            "usort"
            (gitignoreSrc (pkgs.lib.cleanSource ./usort))
            {};
    # Usort for web
    usort-web-client =
        ghcjsPkgs.callCabal2nix
            "usort-web"
            (pkgs.lib.cleanSource ./usort-web)
            {};

    shells = {
        ci = pkgs.mkShell {
            buildInputs = [ pkgs.cachix ];
        };
    };
}
