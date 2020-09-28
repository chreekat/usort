{ sources ? import ./nix/sources.nix
}:
let
    miso = import sources.miso {};
    pkgs = miso.pkgs;
    h = pkgs.haskell.lib;

    gitignoreSrc =
        (import sources."gitignore.nix" {}).gitignoreSource;

    localSrc = dir: gitignoreSrc (pkgs.lib.cleanSource dir);

    extensions = self: super: {
      usort-lib =
        self.callCabal2nix
          "usort-lib"
          (localSrc ./usort-lib)
          {};

      jsaddle-warp = super.jsaddle-warp.overrideAttrs (_:  {
        #src = ../../src/jsaddle/jsaddle-warp;
        patches = [ ./nix/patches/jsaddle.patch ];
      });

      usort-console =
          self.callCabal2nix
              "usort-console"
              (localSrc ./usort-console)
              {};

      usort-web-client =
          self.callCabal2nix
              "usort-web"
              (localSrc ./usort-web)
              {};

      usort-web-jsaddle =
          self.callCabal2nix
              "usort-web"
              (localSrc ./usort-web)
              { miso = miso.miso-jsaddle; };

    };
    reload-script = pkgs.writeScriptBin "reload" ''
      ${ghcPkgs.ghcid}/bin/ghcid -c \
        '${ghcPkgs.cabal-install}/bin/cabal new-repl' \
        -o errors.err \
        -r
    '';

    ghcPkgs = pkgs.haskell.packages.ghc865.extend extensions;
    ghcjsPkgs = pkgs.haskell.packages.ghcjs86.extend extensions;

in rec {

    inherit (ghcPkgs) usort-lib usort-console;

    inherit (ghcjsPkgs) usort-web-client;

    shells = {
        jsaddle = ghcPkgs.usort-web-jsaddle.env.overrideAttrs (old: {
          buildInputs = old.buildInputs ++ [reload-script];
        });

        ci = pkgs.mkShell {
            buildInputs = [ pkgs.cachix ];
        };
    };
}
