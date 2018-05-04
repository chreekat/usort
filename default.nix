{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802" }:
nixpkgs.haskellPackages.callPackage ./package.nix { }
