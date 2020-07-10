{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {}
}:
let
  h = pkgs.haskell.lib;
  gitignoreSrc =
    (import sources."gitignore.nix" { inherit (pkgs) lib;}).gitignoreSource;
in (pkgs.haskellPackages.callCabal2nix "usort" (gitignoreSrc ./.) {})
