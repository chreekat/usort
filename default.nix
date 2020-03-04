{ nixpkgs ? import <nixpkgs> {} }:
let h = nixpkgs.haskell.lib;
in (nixpkgs.haskellPackages.callPackage ./package.nix { })
