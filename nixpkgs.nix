{ sources ? import ./nix/sources.nix
}:
let
    np = sources.nixpkgs;
    pkgs = import np { overlays = [ overlay ]; };

    gitignoreSrc =
      (import sources."gitignore.nix" {}).gitignoreSource;

    localSrc = dir: gitignoreSrc (pkgs.lib.cleanSource dir);

    overlay = self: super: {
      myHaskellPackages = super.haskellPackages.override {
        overrides = hsOverlay self;
      };

      myShell = self.myHaskellPackages.shellFor {
        packages = pkgs: [ pkgs.usort-lib pkgs.usort-console ];
      };
    };

    hsOverlay = pkgs: self: super:
      let
        h = pkgs.haskell.lib;
        unmarkBroken = drv: h.overrideCabal drv (drv: { broken = false; });
      in {
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
in pkgs
