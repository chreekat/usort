{ sources ? import ./nix/sources.nix
}:
let
    miso = import sources.miso {};
    pkgs = miso.pkgs;
    h = pkgs.haskell.lib;

    gitignoreSrc =
        (import sources."gitignore.nix" {}).gitignoreSource;

    addUsortLib = self: super: {
      usort-lib =
        self.callCabal2nix
          "usort-lib"
          (gitignoreSrc (pkgs.lib.cleanSource ./usort-lib))
          {};
    };

    ghcPkgs = pkgs.haskell.packages.ghc865.extend addUsortLib;
    ghcjsPkgs = pkgs.haskell.packages.ghcjs86.extend addUsortLib;

in rec {

    usort-console =
        ghcPkgs.callCabal2nix
            "usort-console"
            (gitignoreSrc (pkgs.lib.cleanSource ./usort-console))
            {};

    usort-web-client =
        ghcjsPkgs.callCabal2nix
            "usort-web"
            (pkgs.lib.cleanSource ./usort-web)
            {};

    inherit (ghcPkgs) usort-lib;

    shells = {
        ci = pkgs.mkShell {
            buildInputs = [ pkgs.cachix ];
        };
    };
}
