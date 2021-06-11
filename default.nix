{ sources ? import ./nix/sources.nix
}:
let
    miso = import sources.miso {};
    pkgs = miso.pkgs;
    h = pkgs.haskell.lib;

    gitignoreSrc =
        (import sources."gitignore.nix" {}).gitignoreSource;

    localSrc = dir: gitignoreSrc (pkgs.lib.cleanSource dir);

    unmarkBroken = drv: h.overrideCabal drv (drv: { broken = false; });

    extensions = self: super: {
      usort-lib =
        self.callCabal2nix
          "usort-lib"
          (localSrc ./usort-lib)
          {};

      jsaddle-warp =
        unmarkBroken
          (h.dontCheck
            (h.appendPatch
              super.jsaddle-warp
              ./nix/patches/jsaddle.patch));

      # overrideAttrs (_:  {
      #   #src = ../../src/jsaddle/jsaddle-warp;
      #   patches = [ ./nix/patches/jsaddle.patch ];
      # });

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

    ghcPkgs = pkgs.haskell.packages.ghc865.override { overrides = extensions; };
    ghcjsPkgs = pkgs.haskell.packages.ghcjs86.override { overrides = extensions; };

in rec {

    inherit (ghcPkgs) usort-lib usort-console usort-web-jsaddle;

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
