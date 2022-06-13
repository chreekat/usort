{ sources ? import ./nix/sources.nix
}:
let
    np = sources.nixpkgs;
    pkgs = import np { inherit config; };
    h = pkgs.haskell.lib;

    gitignoreSrc =
        (import sources."gitignore.nix" {}).gitignoreSource;

    localSrc = dir: gitignoreSrc (pkgs.lib.cleanSource dir);

    unmarkBroken = drv: h.overrideCabal drv (drv: { broken = false; });

    extensions = self: super: {
      termbox-banana = unmarkBroken super.termbox-banana;
      usort-lib =
        self.callCabal2nix
          "usort-lib"
          (localSrc ./usort-lib)
          {};

      usort-console =
          self.callCabal2nix
              "usort-console"
              (localSrc ./usort-console)
              {};
    };

    config = {
      packageOverrides = pkgs: {
        haskellPackages = pkgs.haskellPackages.override {
          overrides = extensions;
        };
      };
    };

in rec {

    inherit (pkgs.haskellPackages) usort-lib usort-console;
}
