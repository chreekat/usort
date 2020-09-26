{ sources ? import ./nix/sources.nix
}:
let
    miso = import sources.miso {};
    pkgs = miso.pkgs;
    h = pkgs.haskell.lib;

    gitignoreSrc =
        (import sources."gitignore.nix" {}).gitignoreSource;

    localSrc = dir: gitignoreSrc (pkgs.lib.cleanSource dir);

    addUsortLib = self: super: {
      usort-lib =
        self.callCabal2nix
          "usort-lib"
          (localSrc ./usort-lib)
          {};
    };

    ghcPkgs = pkgs.haskell.packages.ghc865.extend addUsortLib;
    ghcjsPkgs = pkgs.haskell.packages.ghcjs86.extend addUsortLib;

in rec {

    usort-console =
        ghcPkgs.callCabal2nix
            "usort-console"
            (localSrc ./usort-console)
            {};

    usort-web-client =
        ghcjsPkgs.callCabal2nix
            "usort-web"
            (localSrc ./usort-web)
            {};

    inherit (ghcPkgs) usort-lib;

    shells = {
        jsaddle =
            let
              a = ghcPkgs.callCabal2nix
                "usort-web"
                (localSrc ./usort-web)
                { miso = miso.miso-jsaddle; };
              reload-script = pkgs.writeScriptBin "reload" ''
                ${ghcPkgs.ghcid}/bin/ghcid -c \
                  '${ghcPkgs.cabal-install}/bin/cabal new-repl' \
                  -o errors.err \
                  -T ':main'
              '';
            in a.env.overrideAttrs (old: {
              buildInputs = old.buildInputs ++ [reload-script];
            });

        ci = pkgs.mkShell {
            buildInputs = [ pkgs.cachix ];
        };
    };
}
